-module(heartbeatRequestRPC).
-export([startServer/5, heartbeatRequest/2]).
-define(MIN_HEARTBEAT_TIMEOUT, 150).

% API
startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) ->
    Pid = spawn_link(fun() -> loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) end),
    register(heartbeatRequestLoop, Pid).

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
            case term:getTerm(TermPid) of
                CurrentTerm when CurrentTerm =< TermReq ->
                    % update our term if needed (through lazy evaluation)
                    case (CurrentTerm < TermReq) andalso term:setTerm(TermPid, TermReq) of
                        true ->
                            case state:setFollower(StatePid, TermReq) of
                                true ->
                                    % reset election timer
                                    electionTimer:reset(ElectionTimerPid, TermReq);
                                _ ->
                                    ok
                            end;
                        _ -> 
                            ok
                    end,
                    % try to set the current leader
                    Success = 
                        case currentLeader:setCurrentLeader(CurrentLeaderPid, TermReq, LeaderID) of
                            {LeaderName, LeaderTerm} when LeaderName == LeaderID andalso LeaderTerm == TermReq ->
                                % Valid heartbeat, reset the election timer and disallow votes
                                electionTimer:resetTimer(ElectionTimerPid, TermReq),
                                voteRequestLoop ! {allowVotes, false},
                                true;
                            _ ->
                                false
                        end,  
                    ResponsePid ! {heartbeatResponse, Success, TermReq};
                _ ->
                    % stale request, not granting heartbeat
                    ResponsePid ! {heartbeatResponse, false, term:getTerm(TermPid)}
            end,
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid)

        after ?MIN_HEARTBEAT_TIMEOUT ->
            % allow votes: possibly the leader is gone
            voteRequestLoop ! {allowVotes, true},
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid)
    end.