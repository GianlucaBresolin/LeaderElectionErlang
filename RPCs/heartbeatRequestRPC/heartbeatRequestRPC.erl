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
                    % update our term, revert to follower state, and reset our vote (to update it to the fresher term)
                    case term:setTerm(TermPid, TermReq) of
                        {ok, true} ->
                            state:setFollower(StatePid, TermReq),
                            electionTimer:resetTimer(ElectionTimerPid, TermReq),
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
                case LeaderID == state:setCurrentLeader(CurrentLeaderPid, TermReq, LeaderID) of
                    true ->
                        % Valid heartbeat, reset the election timer and disallow votes
                        electionTimer:resetTimer(ElectionTimerPid, TermReq),
                        voteRequestLoop ! {allowVotes, false},
                        true;
                    false ->
                        false
                end,    
            ResponsePid ! {heartbeatResponse, Success},
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid)
        after ?MIN_HEARTBEAT_TIMEOUT ->
            % allow votes: possibly the leader is gone
            voteRequestLoop ! {allowVotes, true}
    end.