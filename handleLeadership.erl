-module(handleLeadership).
-export([handleLeadership/5]).
-define(RETRAY_DELAY, 20).
-define(HEARTBEAT_TIMEOUT, 20).

% API
handleLeadership(ElectionTimerPid, StatePid, Nodes, Term, MyID) ->
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
            ok
    end,

    spawn(
        fun() -> 
            handleLeadershipLoop(MyID, Term, Nodes, StatePid)
        end).

% Internal stuff
handleLeadershipLoop(LeaderID, Term, Nodes, StatePid) ->
    % provide heartbeats to all nodes
    [spawn(
        fun() -> 
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid)
        end) 
    || NodeID <- Nodes],

    receive
        after ?HEARTBEAT_TIMEOUT ->
            % check if we are still the leader 
            case state:getState(StatePid, Term) of
                {leader, StateTerm} when StateTerm == Term ->
                    % send heartbeats to all nodes after timeout
                    handleLeadershipLoop(LeaderID, Term, Nodes, StatePid);
                _ ->
                    % we are no longer the leader, normal exit
                    ok
            end
    end.

sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid) ->
    Result = rpc:call(NodeID, heartbeatRPC, heartbeat, [Term, LeaderID]),
    case Result of
        {badrpc, _} ->
            % retry (loop back to sendHeartbeat)
            timer:sleep(?RETRAY_DELAY), % avoid busy waiting
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid);
        {ok, Response} ->
            case Response of
                false ->
                    % heartbeat not acknowledged, rever to follower state
                    state:setFollower(StatePid, Term);
                _ ->
                    % heartbeat acknowledged, do nothing
                    ok
            end
    end,
    ok.
