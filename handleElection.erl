-module(handleElection).
-export([handleElection/9]).
-define(RETRAY_DELAY, 20).

% API
handleElection(ElectionTerm, ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, Nodes, BecomeLeaderPid) ->
    % check if the node is already a leader
    case state:getState(StatePid) of
        {ok, leader} ->
            % already the leader, exit
            exit({error, "Already a leader"});
        _ -> 
            % proceed with the election process
            ok
    end,
    
    % reset the election timer to resolve split-votes
    electionTimer:resetTimer(ElectionTimerPid, ElectionTerm),

    % set the new term
    case term:setTerm(TermPid, ElectionTerm) of
        {ok, true} ->
            % term set successfully, proceed
            % (not necessary to reset  myVote here)
            ok;
        {ok, false} ->
            % term not set, exit
            exit({error, "Failed to set term"})
    end,

    % become a candidate 
    case state:setCandidate(StatePid, ElectionTerm) of
        false ->
            % not a candidate, exit
            exit({error, "Failed to become candidate"});
        true ->
            % candidate state set successfully, proceed
            ok
    end,

    % reset the vote count
    voteCount:reset(VoteCountPid, ElectionTerm),

    % set my vote for self (it will update myVote term automatically)
    case myVote:setMyVote(MyVotePid, MyID, ElectionTerm) of
        false ->
            state:setFollower(StatePid, ElectionTerm);
        _ ->
            ok
    end,

    % add my vote to the vote count
    voteCount:addVote(VoteCountPid, MyID, ElectionTerm, BecomeLeaderPid),

    % send vote requests to all other nodes (in parallel)
    [spawn(fun() -> askVoteLoop(NodeID, MyID, ElectionTerm, VoteCountPid, BecomeLeaderPid) end) || NodeID <- Nodes],
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
    end.