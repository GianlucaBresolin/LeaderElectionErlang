-module(term).
-export([start/0, inc/2, setTerm/2, getTerm/2]).

% API
start() ->
    spawn( fun() -> loop(0) end ).

setTerm(Pid, NewTerm) ->
    Pid ! {set, NewTerm, self()},
    receive
        {success, Result} -> Result
    end.

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
            
        {set, NewTerm, ResponsePid} ->
            case NewTerm > Term of
                true ->
                    ResponsePid ! {success, true},
                    loop(NewTerm);
                false ->
                    ResponsePid ! {success, false},
                    loop(Term)
            end;
            
        {get, ResponsePid} ->
            ResponsePid ! {term, Term},
            loop(Term)
    end.