-module(state).
-export([start/0, setFollower/2, setCandidate/2, setLeader/2, getState/1]).

% API
start() ->
    spawn_link(fun() -> loop(follower, 0) end).

setFollower(Pid, Term) ->
    Pid ! {setFollower, Term, self()},
    receive
        {becomeFollowerSuccess, Success} -> Success
    end.

setCandidate(Pid, Term) ->
    Pid ! {setCandidate, Term, self()},
    receive
        {becomeCandidateSuccess, Success} -> Success
    end.

setLeader(Pid, Term) ->
    Pid ! {setLeader, Term, self()},
    receive 
        {becomeLeaderSuccess, Response} -> Response
    end.

getState(Pid) ->
    Pid ! {getState, self()},
    receive
        {state, State, Term} -> {State, Term}
    end.

% Internal loop
loop(State, Term) ->
    receive
        {setFollower, NewTerm, From} ->
            UpdatedState =
                case NewTerm >= Term of 
                    true -> 
                        case State of 
                            follower -> 
                                From ! {becomeFollowerSuccess, false};
                            _ ->
                                From ! {becomeFollowerSuccess, true}
                        end,
                        follower;
                    false -> 
                        From ! {becomeFollowerSuccess, false},
                        State
                end,
            loop(UpdatedState, erlang:max(Term, NewTerm));

        {setCandidate, NewTerm, ResponsePid} ->
            UpdatedState =
                if
                    NewTerm > Term ->
                        ResponsePid ! {becomeCandidate, true},
                        candidate;
                    NewTerm == Term ->
                        case State of
                            follower -> 
                                ResponsePid ! {becomeCandidate, true},
                                candidate;
                            _ -> 
                                ResponsePid ! {becomeCandidate, false}, 
                                State
                        end;
                    NewTerm < Term ->
                        State
                end,
            loop(UpdatedState, erlang:max(Term, NewTerm));

        {setLeader, NewTerm, ResponsePid} ->
            UpdatedState =
                if 
                    NewTerm > Term ->
                        ResponsePid ! {becomeLeader, true},
                        leader;
                    NewTerm == Term ->
                        case State =/= leader of
                            true ->
                                ResponsePid ! {becomeLeader, true};
                            false ->
                                % already leader
                                ResponsePid ! {becomeLeader, false} 
                        end,
                        leader;
                    NewTerm < Term ->
                        % stale request
                        ResponsePid ! {becomeLeader, false}, 
                        State                    
                end,
            loop(UpdatedState, erlang:max(Term, NewTerm));

        {getState, ResponsePid} ->
            ResponsePid ! {state, State, Term},
            loop(State, Term)
    end.