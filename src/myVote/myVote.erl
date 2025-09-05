-module(myVote).
-export([start/0, setMyVote/3]).

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
        {setMyVote, VoteReq, TermReq, ResponsePid} -> 
            {Success, UpdatedVote} =
                case TermReq of 
                    TermReq when TermReq > Term  ->
                        % new term, update vote
                        {true, VoteReq};
                    TermReq when TermReq == Term andalso (MyVote == undefined orelse MyVote == VoteReq) ->
                        % success (idempotent solution)
                        {true, VoteReq};
                    _ ->
                        % stale request or already voted for someone else
                        {false, MyVote}
                end,
            ResponsePid ! {setMyVoteSuccess, Success},
            loop(UpdatedVote, erlang:max(TermReq, Term))
    end.