-module(heartbeatRequestRPC).
-export([startServer/5, heartbeatRequest/2]).
-define(MIN_HEARTBEAT_TIMEOUT, 150).

% API
startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) ->
    Pid = spawn_link(fun() -> loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) end),
    register(heartbeatRequestLoop, Pid),
    {ok, Pid}.

% RPC
heartbeatRequest(Term, LeaderID) ->
    heartbeatRequestLoop ! {heartbeatRequest, Term, LeaderID, self()},
    receive 
        {heartbeatResponse, Granted, ResponseTerm} -> {Granted, ResponseTerm}
    end.

% Internal loop
loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) ->
    receive 
        {heartbeatRequest, TermReq, LeaderID, ResponsePid} ->
            % get our current term
            CurrentTerm = term:getTerm(TermPid, self()),
                
            case CurrentTerm > TermReq of
                true ->
                    % stale request, not granting heartbeat
                    ResponsePid ! {heartbeatResponse, false, CurrentTerm};
                false ->
                    case CurrentTerm < TermReq of
                        true ->
                            % update our term
                            case term:setTerm(TermPid, TermReq) of
                                {ok, true} ->
                                    case state:setFollower(StatePid, TermReq) of
                                        {ok, true} ->
                                            % reset election timer
                                            electionTimer:reset(ElectionTimerPid, TermReq);
                                        _ ->
                                            ok
                                    end;
                                {ok, false} -> 
                                    % in the while, another request came in with > term, ignore this one
                                    LatestTerm = term:getTerm(TermPid, self())
                                    ResponsePid ! {heartbeatResponse, false, LatestTerm};        
                            end;
                        false ->
                            % CurrentTerm == TermReq: proceed to set the current leader
                            ok
                    end;
                    % try to set the current leader
                    Success = 
                        case LeaderID == state:setCurrentLeader(CurrentLeaderPid, TermReq, LeaderID) of
                            {ok, true} ->
                                % Valid heartbeat, reset the election timer and disallow votes
                                electionTimer:resetTimer(ElectionTimerPid, TermReq),
                                voteRequestLoop ! {allowVotes, false},
                                true;
                            _ ->
                                false
                        end,    
                    ResponsePid ! {heartbeatResponse, Success, TermReq},
            end,
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid);

        after ?MIN_HEARTBEAT_TIMEOUT ->
            % allow votes: possibly the leader is gone
            voteRequestLoop ! {allowVotes, true}
    end.