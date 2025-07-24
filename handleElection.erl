-module(handleElection).
-export([handleElection/8]).
-define(RETRAY_DELAY, 20).

% API
handleElection(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, Nodes, BecomeLeaderPid) ->
    % reset the election timer to resolve split-votes
    electionTimer:reset(ElectionTimerPid),

    % increment the current term
    NewTerm = term:inc(TermPid, self()),   

    % become a candidate 
    state:setCandidate(StatePid, NewTerm),

    % reset the vote count
    voteCount:reset(VoteCountPid, NewTerm),

    % set my vote for self
    case myVote:setMyVote(MyVotePid, MyID, NewTerm) of
        false ->
            state:setFollower(StatePid, none, ElectionTimerPid, none, NewTerm);
        _ ->
            ok
    end,

    % add my vote to the vote count
    voteCount:addVote(VoteCountPid, MyID, NewTerm, BecomeLeaderPid),

    % send vote requests to all other nodes (in parallel)
    [spawn(fun() -> askVoteLoop(NodeID, MyID, NewTerm, VoteCountPid, BecomeLeaderPid) end) || NodeID <- Nodes],
    ok.

% Internal stuff
askVoteLoop(NodeID, CandidateID, Term, VoteCountPid, BecomeLeaderPid) ->
    Result = rpc:call(NodeID, voteRequestRPC, voteRequest, [Term, CandidateID]),
    case Result of 
        {badrpc, _} ->
            % retry (loop back to askVote)
            timer:sleep(?RETRAY_DELAY), % avoid busy waiting
            askVoteLoop(NodeID, CandidateID, Term, VoteCountPid, BecomeLeaderPid);
        {ok, Granted} ->
            case Granted of
                true ->
                    % vote granted: add the vote to the count
                    voteCount:addVote(VoteCountPid, NodeID, Term, BecomeLeaderPid),
                    ok;
                _ ->
                    % vote not granted
                    ok
            end
    end,
    ok.