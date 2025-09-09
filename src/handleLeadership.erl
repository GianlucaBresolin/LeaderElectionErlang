-module(handleLeadership).
-export([handleLeadership/6]).
-define(RETRAY_DELAY, 20).
-define(HEARTBEAT_TIMEOUT, 20).

% API
handleLeadership(ElectionTimerPid, StatePid, TermPid, Nodes, Term, MyID) ->
    % stop the election timer
    electionTimer:stopTimer(ElectionTimerPid, Term),

    % set the state to leader
    case state:setLeader(StatePid, Term) of
        false ->
            % restart the election timer before exiting
            electionTimer:reset(ElectionTimerPid, Term),
            exit({error, "Failed to set leader state"});
        true ->
            % leader state set successfully, proceed
            io:format("NODE ~p BECAME LEADER FOR TERM ~p~n", [MyID, Term])
    end,

    spawn(
        fun() -> 
            handleLeadershipLoop(MyID, Term, Nodes, StatePid, ElectionTimerPid, TermPid)
        end).

% Internal stuff
handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, ElectionTimerPid, TermPid) ->
    % provide heartbeats to all nodes
    [spawn(
        fun() -> 
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid, TermPid)
        end) 
    || {NodeID} <- Nodes],

    receive
        after ?HEARTBEAT_TIMEOUT ->
            % check if we are still the leader 
            case state:getState(StatePid) of
                {leader, StateTerm} when StateTerm == Term ->
                    % send heartbeats to all nodes after timeout
                    handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, ElectionTimerPid, TermPid);
                _ ->
                    % we are no longer the leader, normal exit
                    ok
            end
    end.

sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid, TermPid) ->
    case NodeID =/= LeaderID of
        true ->
            RemoteNode = NodeID ++ "@" ++ NodeID,
            Result = rpc:call(list_to_atom(RemoteNode), heartbeatRequestRPC, heartbeatRequest, [Term, LeaderID]),
            case Result of
                {badrpc, _} ->
                    % retry (loop back to sendHeartbeat)
                    timer:sleep(?RETRAY_DELAY), % avoid busy waiting
                    sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid, TermPid);
                {Granted, ResponseTerm} ->
                    case Granted of
                        false ->
                            % heartbeat not acknowledged, try to update the term and rever to follower
                            case term:setTerm(TermPid, ResponseTerm) of
                                true ->
                                    % revert to follower state
                                    case state:setFollower(StatePid, Term) of
                                        true ->
                                            % reset the election timer
                                            electionTimer:resetTimer(ElectionTimerPid, Term);
                                        _ -> 
                                            ok
                                    end;
                                _ ->
                                    ok
                            end;
                        _ ->
                            % heartbeat acknowledged, do nothing
                            ok
                    end
            end;
        _ ->
            % we don't send heartbeat to ourselves
            ok
    end.

