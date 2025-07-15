-module(myVote).
-export([start/0, setMyVote/3, reset/2]).

% API
start() ->
    spawn(fun() -> loop(undefined, 0) end).

setMyVote(Pid, Vote, Term) -> 
    Pid ! {setMyVote, Vote, Term, self()},
    receive
        {operation, Success} -> Success
    end.

reset(Pid, Term) -> 
    Pid ! {reset, Term},
    ok.

% Internal loop
loop(MyVote, Term) ->
    receive 
        {setMyVote, Vote, NewTerm, ResponsePid} -> 
            case NewTerm < Term or (NewTerm == Term andalso Vote =/= undefined) of 
                true ->
                    % term is not valid or we already voted
                    ResponsePid ! {operation, false},
                    loop(Vote, Term);
                false ->
                    % valid term, update vote and term
                    ResponsePid ! {operation, true},
                    loop(Vote, NewTerm)
            end;
        {reset, ResetTerm} ->
            case ResetTerm > Term of
                true -> 
                    loop(undefined, ResetTerm);
                false ->
                    % stale request, ignore
                    loop(MyVote, Term)
            end
    end.