-module(voteRequestRPC).
-export([startServer/3, voteRequest/2]).

% API
startServer(TermPid, StatePid, MyVotePid) ->
    Pid = spawn(fun() -> loop(TermPid, StatePid, MyVotePid) end),
    register(voteRequestLoop, Pid),
    {ok, Pid}.

% RPC
voteRequest(Term, CandidateID) ->
    voteRequestLoop ! {voteRequest, Term, CandidateID, self()},
    receive 
        {voteResponse, Granted} -> Granted
    end.

% Internal loop
loop(TermPid, StatePid, MyVotePid) ->
    receive
        {voteRequest, TermReq, CandidateID, ResponsePid} ->
            % get our current term
            CurrentTerm = term:getTerm(TermPid, self()),
                
            if 
                CurrentTerm > TermReq ->
                    % stale request, not granting vote
                    ResponsePid ! {voteResponse, false},
                    loop(TermPid, StatePid, MyVotePid);
                CurrentTerm > TermReq ->
                    % update our term
                    case term:setTerm(TermPid, TermReq) of 
                        true ->
                            % revert to follower state
                            state:setFollower(StatePid, none, none, none, TermReq);
                            % proceed to set our vote
                        false -> % in the while, another request came in with >= term, ignore this one
                            ResponsePid ! {voteResponse, false},
                            loop(TermPid, StatePid, MyVotePid)
                    end;
                CurrentTerm == TermReq ->
                    % prooced to set our vote
                    ok
            end,
            %try to set our vote
            case myVote:setMyVote(MyVotePid, CandidateID, TermReq) of
                false ->
                    % not grant the vote
                    ResponsePid ! {voteResponse, false},
                    loop(TermPid, StatePid, MyVotePid);
                true ->
                    % grant the vote
                    ResponsePid ! {voteResponse, true},
                    loop(TermPid, StatePid, MyVotePid)
            end
    end.