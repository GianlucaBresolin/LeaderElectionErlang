-module(term).
-export([start/0, inc/2, setTerm/2, getTerm/2]).

% API
start() ->
    spawn( fun() -> loop(0) end ).

inc(Pid) ->
    Pid ! {inc, self()},
    receive
        {term, NewTerm} -> NewTerm
    end.

setTerm(Pid, NewTerm) ->
    Pid ! {set, NewTerm},
    ok.

getTerm(Pid) -> 
    Pid ! {get, self()},
    receive
        {term, Term} -> Term
    end.

% Internal loop
loop(Term) ->
    receive
        {inc, ResponsePid} ->
            NewTerm = Term + 1,
            ResponsePid ! {term, NewTerm},
            loop(NewTerm);
            
        {set, NewTerm} ->
            case NewTerm > Term of
                true ->
                    loop(NewTerm);
                false ->
                    loop(Term)
            end;
            
        {get, ResponsePid} ->
            ResponsePid ! {term, Term},
            loop(Term)
    end.