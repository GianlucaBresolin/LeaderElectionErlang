-module(state).
-export([start/0, setFollower/5, setCandidate/2, setLeader/3]).

% API
start() ->
    spawn(fun() -> loop(follower, 0) end).

setFollower(Pid, HeartbeatTimerPid, ElectionTimerPid, StopLeadershipPid, Term) ->
    Pid ! {setFollower, HeartbeatTimerPid, ElectionTimerPid, StopLeadershipPid, Term},
    ok.

setCandidate(Pid, Term) ->
    Pid ! {setCandidate, Term},
    ok.

setLeader(Pid, Term) ->
    Pid ! {setLeader, Term, self()},
    receive 
        {becomeLeader, Response} -> Response
    end.

% Internal loop
loop(State, Term) ->
    receive
        {setFollower, HeartbeatTimerPid, ElectionTimerPid, StopLeadershipPid, NewTerm} ->
            if 
                NewTerm >= Term ->
                    case State == leader of
                        true ->
                            case is_process_alive(HeartbeatTimerPid) andalso is_process_alive(ElectionTimerPid) andalso is_process_alive(StopLeadershipPid) of
                                true ->
                                    heartbeatTimer:stop(HeartbeatTimerPid),
                                    electionTimer:stop(ElectionTimerPid),
                                    StopLeadershipPid ! stopLeadership; % TODO: check it
                                false ->
                                    exit({error, "Heartbeat or Election timer or StopLeadership process not alive"})
                            end;    
                        _ ->
                            ok
                    end,
                    UpdatedTerm = NewTerm,
                    UpdatedState = follower;
                NewTerm < Term ->
                    UpdatedState = State,
                    UpdatedTerm = Term
            end,
            loop(UpdatedState, UpdatedTerm);

        {setCandidate, NewTerm} ->
            if 
                NewTerm > Term ->
                    UpdatedState = candidate,
                    UpdatedTerm = NewTerm;
                NewTerm == Term ->
                    UpdatedTerm = NewTerm,
                    UpdatedState = case State of
                        follower -> candidate;
                        candidate -> candidate;
                        leader -> leader
                    end;
                NewTerm < Term ->
                    UpdatedState = State,
                    UpdatedTerm = Term
            end,
            loop(UpdatedState, UpdatedTerm);

        {setLeader, NewTerm, ResponsePid} ->
            if 
                NewTerm > Term ->
                    UpdatedState = leader,
                    UpdatedTerm = NewTerm,
                    ResponsePid ! {becomeLeader, true};
                NewTerm == Term ->
                    case State =/= leader of
                        true ->
                            ResponsePid ! {becomeLeader, true};
                        false ->
                            ResponsePid ! {becomeLeader, false} % already leader
                    end,
                    UpdatedState = leader,
                    UpdatedTerm = NewTerm;
                NewTerm < Term ->
                    UpdatedState = State,
                    UpdatedTerm = Term,
                    ResponsePid ! {becomeLeader, false} % not leader
            end,
            loop(UpdatedState, UpdatedTerm)
    end.