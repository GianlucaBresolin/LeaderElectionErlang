-module(state).
-export([start/0, setFollower/2, setCandidate/2, setLeader/2, getState/1]).

% API
start() ->
    spawn(fun() -> loop(follower, 0) end).

setFollower(Pid, Term) ->
    Pid ! {setFollower, Term},
    ok.

setCandidate(Pid, Term) ->
    Pid ! {setCandidate, Term, self()},
    receive
        {becomeCandidate, Response} -> Response
    end.

setLeader(Pid, Term) ->
    Pid ! {setLeader, Term, self()},
    receive 
        {becomeLeader, Response} -> Response
    end.

getState(Pid) ->
    Pid ! {getState, self()},
    receive
        {state, State, Term} -> {State, Term}
    end.

% Internal loop
loop(State, Term) ->
    receive
        {setFollower, NewTerm} ->
            UpdatedState =
                case NewTerm >= Term of 
                    true -> follower;
                    false -> State
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
                            candidate -> 
                                ResponsePid ! {becomeCandidate, false}, 
                                candidate;
                            leader -> 
                                ResponsePid ! {becomeCandidate, false},
                                leader
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
                                ResponsePid ! {becomeLeader, false} % already leader
                        end,
                        leader;
                    NewTerm < Term ->
                        ResponsePid ! {becomeLeader, false}, % not a valid term for becoming leader
                        State                    
                end,
            loop(UpdatedState, erlang:max(Term, NewTerm));

        {getState, ResponsePid} ->
            ResponsePid ! {state, State, Term},
            loop(State, Term)
    end.