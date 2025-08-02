-module(heartbeatRequestRPC).
-export([startServer/5, heartbeatRequest/2]).

% API
startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) ->
    Pid = spawn(fun() -> loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) end),
    register(heartbeatRequestLoop, Pid),
    {ok, Pid}.

% RPC
heartbeatRequest(Term, LeaderID) ->
    heartbeatRequestLoop ! {heartbeatRequest, Term, LeaderID, self()},
    receive 
        {heartbeatResponse, Granted} -> Granted
    end.

% Internal loop
loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid) ->
    receive 
        {heartbeatRequest, TermReq, LeaderID, ResponsePid} ->
            % get our current term
            CurrentTerm = term:getTerm(TermPid, self()),
                
            if 
                CurrentTerm > TermReq ->
                    % stale request, not granting heartbeat
                    ResponsePid ! {heartbeatResponse, false},
                    loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid);
                CurrentTerm < TermReq ->
                    % update our term, revert to follower state, and reset our vote (to update it to the best term)
                    case term:setTerm(TermPid, TermReq) of
                        {ok, true} ->
                            state:setFollower(StatePid, none, none, none, TermReq),
                            myVote:resetMyVote(MyVotePid, TermReq);
                        {ok, false} -> % in the while, another request came in with >= term, ignore this one
                            ResponsePid ! {heartbeatResponse, false},
                            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid)
                    end;
                CurrentTerm == TermReq ->
                    % proceed to set the current leader
                    ok
            end,

            % try to set the current leader
            Success = 
                case LeaderID == state:setCurrentLeader(CurrentLeaderPid, LeaderID) of
                    true ->
                        % reset the election timer
                        electionTimer:resetTimer(ElectionTimerPid, TermReq),
                        true;
                    false ->
                        false
                end,    
            ResponsePid ! {heartbeatResponse, Success},
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid)
    end.