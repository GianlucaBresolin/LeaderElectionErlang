-module(handleLeadership).
-export([handleLeadership/5]).
-define(RETRAY_DELAY, 20).

% API
handleLeadership(ElectionTimerPid, StatePid, Nodes, Term, MyID) ->
    % stop the election timer
    electionTimer:stop(ElectionTimerPid),

    % set the state to leader
    case state:setLeader(StatePid, Term) of
        {ok, true} ->
            % proceed with leadership
            ok;
        {ok, false} ->
            % restart the election timer before exiting
            electionTimer:reset(ElectionTimerPid),
            exit({error, "Failed to set leader state"});
        {error, Reason} ->
            exit({error, Reason})
    end,

    spawn(
        fun() -> 
            handleLeadershipLoop(MyID, Term, Nodes, StatePid, ElectionTimerPid)
        end).

% Internal stuff
handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, ElectionTimerPid) ->
    % provide heartbeats to all nodes
    [spawn(
        fun() -> 
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid)
        end) 
    || NodeID <- Nodes],

    receive
        {stopLeadership} ->
            % stop handleLeadership loop
            ok
        after 10 ->
            % send heartbeats to all nodes after timeout
            handleLeadershipLoop(LeaderID, Term, Nodes, StatePid, ElectionTimerPid)
    end.

sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid) ->
    Result = rpc:call(NodeID, heartbeatRPC, heartbeat, [Term, LeaderID]),
    case Result of
        {badrpc, _} ->
            % retry (loop back to sendHeartbeat)
            timer:sleep(?RETRAY_DELAY), % avoid busy waiting
            sendHeartbeatLoop(NodeID, LeaderID, Term, StatePid, ElectionTimerPid);
        {ok, Response} ->
            case Response of
                false ->
                    % heartbeat not acknowledged, rever to follower state
                    state:setFollower(StatePid, ElectionTimerPid, self(), Term);
                _ ->
                    % heartbeat acknowledged, do nothing
                    ok
            end
    end,
    ok.
