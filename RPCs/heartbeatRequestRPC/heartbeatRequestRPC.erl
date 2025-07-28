-module(heartbeatRequestRPC).
-export([startServer/4, heartbeatRequest/2]).

% API
startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid) ->
    Pid = spawn(fun() -> loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid) end),
    register(heartbeatRequestLoop, Pid),
    {ok, Pid}.

% RPC
heartbeatRequest(Term, LeaderID) ->
    heartbeatRequestLoop ! {heartbeatRequest, Term, LeaderID, self()},
    receive 
        {heartbeatResponse, Granted} -> Granted
    end.

% Internal loop
loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid) ->
    receive 
        {heartbeatRequest, TermReq, LeaderID, ResponsePid} ->
            % get our current term
            CurrentTerm = term:getTerm(TermPid, self()),
                
            if 
                CurrentTerm > TermReq ->
                    % stale request, not granting heartbeat
                    ResponsePid ! {heartbeatResponse, false},
                    loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid); 
                CurrentTerm < TermReq ->
                    % update our term, revert to follower state, and reset our vote (to update it to the best term)
                    term:setTerm(TermPid, TermReq),
                    state:setFollower(StatePid, none, none, none, TermReq),
                    myVote:resetMyVote(MyVotePid, TermReq);
                    % proceed to set the current leader
                CurrentTerm == TermReq ->
                    % proceed to set the current leader
                    ok
            end,

            % try to set the current leader
            ResponsePid ! {heartbeatResponse, (LeaderID == state:setCurrentLeader(CurrentLeaderPid, LeaderID))},
            loop(TermPid, StatePid, CurrentLeaderPid, MyVotePid)
    end.