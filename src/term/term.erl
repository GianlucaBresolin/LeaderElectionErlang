-module(term).
-export([start/0, setTerm/2, getTerm/1]).

% API
start() ->
    spawn_link( fun() -> loop(0) end ).

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
        {set, NewTerm, ResponsePid} ->
            case NewTerm >= Term of
                true ->
                    ResponsePid ! {success, true},
                false ->
                    ResponsePid ! {success, false}
            end;
            loop(erlang:max(Term, NewTerm));
            
        {get, ResponsePid} ->
            ResponsePid ! {term, Term},
            loop(Term)
    end.