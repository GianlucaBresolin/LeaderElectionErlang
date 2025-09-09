-module(handleElection).
-export([handleElection/9]).
-define(RETRAY_DELAY, 20).

% API
handleElection(ElectionTerm, ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, Nodes, BecomeLeaderPid) ->
    % check if the node is already a leader
    case state:getState(StatePid) of
        {leader, _} ->
            % already the leader, exit election
            exit({error, "Already a leader."});
        {_, StateTerm} when StateTerm > ElectionTerm ->
            exit({error, "Stale election term."});
        _ -> 
            % proceed with the election process
            ok
    end,

    io:format("NODE ~s START ELECTION FOR TERM ~p~n", [MyID, ElectionTerm]),
    
    % set the term for the election
    case term:setTerm(TermPid, ElectionTerm) of
        true ->
            ok;
        false ->
            % term not set, exit
            exit({error, "Failed to set term"})
    end,

    % reset the election timer to resolve possible split-votes
    electionTimer:resetTimer(ElectionTimerPid, ElectionTerm),   

    % become a candidate 
    case state:setCandidate(StatePid, ElectionTerm) of
        false ->
            % failed to become candidate, exit
            exit({error, "Failed to become candidate"});
        true ->
            % candidate state set successfully, proceed
            ok
    end,

    % set my vote for self (it will update myVote term automatically)
    case myVote:setMyVote(MyVotePid, MyID, ElectionTerm) of
        true ->
            % add our vote to the vote count
            voteCount:addVote(VoteCountPid, MyID, ElectionTerm, BecomeLeaderPid);
        _ ->
            ok
    end,

    % send vote requests to all other nodes (in parallel)
    [spawn(fun() -> askVoteLoop(NodeID, MyID, ElectionTerm, VoteCountPid, BecomeLeaderPid) end) || {NodeID} <- Nodes],
    ok.

% Internal stuff
askVoteLoop(NodeID, CandidateID, Term, VoteCountPid, BecomeLeaderPid) ->
    case NodeID =/= CandidateID of 
        true ->
            RemoteNode = NodeID ++ "@" ++ NodeID,
            Result = rpc:call(list_to_atom(RemoteNode), voteRequestRPC, voteRequest, [Term, CandidateID]),
            case Result of 
                {badrpc, _} ->
                    % retry (loop back to askVote)
                    timer:sleep(?RETRAY_DELAY), % avoid busy waiting
                    askVoteLoop(NodeID, CandidateID, Term, VoteCountPid, BecomeLeaderPid);
                true ->
                    % vote granted: add the vote to the count
                    voteCount:addVote(VoteCountPid, NodeID, Term, BecomeLeaderPid);
                _ ->
                    % vote not granted
                    ok
            end;
        _ ->
            % already voted for self
            ok
    end.