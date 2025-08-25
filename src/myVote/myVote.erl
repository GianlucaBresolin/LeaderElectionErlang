-module(myVote).
-export([start/0, setMyVote/3, reset/2]).

% API
start() ->
    spawn_link(fun() -> loop(undefined, 0) end).

setMyVote(Pid, Vote, Term) -> 
    Pid ! {setMyVote, Vote, Term, self()},
    receive
        {setMyVoteSuccess, Success} -> Success
    end.

% Internal loop
loop(MyVote, Term) ->
    receive 
        {setMyVote, Vote, NewTerm, ResponsePid} -> 
            case NewTerm < Term or (NewTerm == Term andalso Vote =/= undefined) of 
                true ->
                    % term is not valid or we already voted
                    ResponsePid ! {setMyVoteSuccess, false},
                    loop(Vote, Term);
                false ->
                    % valid term, update vote and term
                    ResponsePid ! {setMyVoteSuccess, true},
                    loop(Vote, NewTerm)
            end
    end.