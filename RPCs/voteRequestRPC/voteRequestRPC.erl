-module(voteRequestRPC).
-export([startServer/4, voteRequest/2]).

% API
startServer(TermPid, StatePid, ElectionTimerPid, MyVotePid) ->
    Pid = spawn(fun() -> loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, true) end),
    register(voteRequestLoop, Pid),
    {ok, Pid}.

% RPC
voteRequest(Term, CandidateID) ->
    voteRequestLoop ! {voteRequest, Term, CandidateID, self()},
    receive 
        {voteResponse, Granted} -> Granted
    end.

% Internal loop
loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes) ->
    receive
        {voteRequest, TermReq, CandidateID, ResponsePid} ->
            case AllowVotes of
                false ->
                    % votes not allowed, ignore the request
                    ResponsePid ! {voteResponse, false},
                    loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes);
                true ->
                    % get our current term
                    CurrentTerm = term:getTerm(TermPid, self()),
                        
                    if 
                        CurrentTerm > TermReq ->
                            % stale request, not granting vote
                            ResponsePid ! {voteResponse, false},
                            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes);
                        CurrentTerm > TermReq ->
                            % update our term
                            case term:setTerm(TermPid, TermReq) of 
                                true ->
                                    % revert to follower state
                                    state:setFollower(StatePid, TermReq),
                                    electionTimer:reset(ElectionTimerPid, TermReq);
                                    % proceed to set our vote (not necessary to reset myVote here)
                                false -> % in the while, another request came in with >= term, ignore this one
                                    ResponsePid ! {voteResponse, false},
                                    loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes)
                            end;
                        CurrentTerm == TermReq ->
                            % prooced to set our vote
                            ok
                    end,
                    %try to set our vote (it will update myVote term automatically)
                    case myVote:setMyVote(MyVotePid, CandidateID, TermReq) of
                        false ->
                            % not grant the vote
                            ResponsePid ! {voteResponse, false},
                            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes);
                        true ->
                            % grant the vote
                            ResponsePid ! {voteResponse, true},
                            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes)
                    end
            end;
        {allowVotes, Value} ->
            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, Value);
    end.