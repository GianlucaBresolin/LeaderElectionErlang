-module(handleLeadership).
-export([handleLeadership/5]).
-define(RETRAY_DELAY, 20).

% API
handleLeadership(ElectionTimerPid, StatePid, Nodes, Term, MyID) ->
    % stop the election timer
    electionTimer:stop(ElectionTimerPid),

    % set the state to leader
    state:setLeader(StatePid, Term),

    % start the heartbeat timer
    HeartbeatTimerPid = heartbeatTimer:startTimer(self()),

    % provide heartbeats for the first time (before the first timeout)
    [spawn(
        fun() -> 
            sendHeartbeatLoop(NodeID, MyID, Term, StatePid, HeartbeatTimerPid, ElectionTimerPid)
        end) 
    || NodeID <- Nodes],

    handleLeadershipLoop(MyID, Term, Nodes, StatePid, HeartbeatTimerPid, ElectionTimerPid).

% Internal stuff
handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, HeartbeatTimerPid, ElectionTimerPid) ->
    receive
        {heartbeatTimeoutSignal} ->
            % send heartbeats to all nodes
            [spawn(
                fun() -> 
                    sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, HeartbeatTimerPid, ElectionTimerPid) 
                end) 
            || NodeID <- Nodes],
            handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, HeartbeatTimerPid, ElectionTimerPid);
        {stopLeadership} ->
            ok
    end.

sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, HeartbeatTimerPid, ElectionTimerPid) ->
    Result = rpc:call(NodeID, heartbeatRPC, heartbeat, [Term, LeaderID]),
    case Result of
        {badrpc, _} ->
            % retry (loop back to sendHeartbeat)
            timer:sleep(?RETRAY_DELAY), % avoid busy waiting
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, HeartbeatTimerPid, ElectionTimerPid);
        {ok, Response} ->
            case Response of
                false ->
                    % heartbeat not acknowledged, rever to follower state
                    state:setFollower(StatePid, HeartbeatTimerPid, ElectionTimerPid, self(), Term);
                _ ->
                    % heartbeat acknowledged, do nothing
                    ok
            end
    end,
    ok.
