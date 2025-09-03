-module(voteRequestRPC).
-export([startServer/4, voteRequest/2]).

% API
startServer(TermPid, StatePid, ElectionTimerPid, MyVotePid) ->
    Pid = spawn_link(fun() -> loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, true) end),
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
                    ResponsePid ! {voteResponse, false};
                true ->
                    case state:getState(StatePid) of 
                        {ok, Leader, _} when Leader == leader ->
                            % we are the leader, ignore the request (AllowVotes should be false in this case)
                            ResponsePid ! {voteResponse, false};
                        _ ->
                            % get our current term
                            case term:getTerm(TermPid) of
                                {ok, CurrentTerm} when CurrentTerm =< TermReq ->
                                    % update our term if neeeded (through lazy evaluation)
                                    case (CurrentTerm < TermReq) andalso term:setTerm(TermPid, TermReq) of 
                                        true ->
                                            % revert to follower state
                                            case state:setFollower(StatePid, TermReq) of 
                                                {ok, true} ->
                                                    electionTimer:reset(ElectionTimerPid, TermReq);
                                                _ ->
                                                    ok
                                            end;
                                        _ ->     
                                            ok
                                    end,
                                    %try to set our vote
                                    case myVote:setMyVote(MyVotePid, CandidateID, TermReq) of
                                        false ->
                                            % not grant the vote
                                            ResponsePid ! {voteResponse, false};
                                        true ->
                                            % grant the vote
                                            ResponsePid ! {voteResponse, true}
                                    end;
                                _ ->
                                    % stale request, not granting vote
                                    ResponsePid ! {voteResponse, false}
                            end
                    end
            end,
            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, AllowVotes);

        {allowVotes, Value} ->
            loop(TermPid, StatePid, ElectionTimerPid, MyVotePid, Value)
    end.