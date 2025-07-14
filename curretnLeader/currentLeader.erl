-module(currentLeader).
-export([start/0, setCurrentLeader/3, reset/2]).

% API
start() ->
    spawn(fun() -> loop(undefined, 0) end).

setCurrentLeader(Pid, Term, Leader) ->
    Pid ! {setCurrentLeader, self(), Term, Leader},
    receive
        {currentLeader, LeaderName} -> LeaderName
    end.

reset(Pid, Term) ->
    Pid ! {reset, Term},
    ok.

% Internal loop
loop(CurrentLeader, CurrentTerm) ->
    receive
        % set current leader
        {setCurrentLeader, From, Term, Leader} ->
            NewLeader =
                case Term > CurrentTerm of
                    true -> Leader;
                    false ->
                        case Term == CurrentTerm andalso CurrentLeader == undefined of
                            true -> Leader;
                            false -> CurrentLeader
                        end
                end,
            NewTerm = erlang:max(CurrentTerm, Term),
            From ! {currentLeader, NewLeader},
            loop(NewLeader, NewTerm);
        
        % reset current leader
        {reset, Term} ->
            NewState = case Term > CurrentTerm of
                           true -> {undefined, Term};
                           false -> {CurrentLeader, CurrentTerm}
                       end,
            loop(element(1, NewState), element(2, NewState))
    end.