-module(electionTimer).
-export([startTimer/1, resetTimer/2, stopTimer/2, setMinTimeout/2, setMaxTimeout/2]).

% API
startTimer(ResponsePid) ->  
    Pid = spawn(fun() -> loop(150, 300, undefined, 0, ResponsePid) end), % default min = 150, max = 300
    Pid ! startSignal,
    Pid.
    
resetTimer(Pid, Term) -> 
    Pid ! {resetSignal, Term},
    ok.

stopTimer(Pid, Term) ->
    Pid ! {stopSignal, Term},
    ok.

setMinTimeout(Pid, MinTimeout) ->
    Pid ! {minTimeout, MinTimeout},
    ok.

setMaxTimeout(Pid, MaxTimeout) ->
    Pid ! {maxTimeout, MaxTimeout},
    ok.


% Internal loop
loop(MinTimeout, MaxTimeout, TimerRef, Term, ResponsePid) ->
    receive 
        {minTimeout, NewMinTimeout} ->
            loop(NewMinTimeout, MaxTimeout, TimerRef, Term, ResponsePid);

        {maxTimeout, NewMaxTimeout} ->
            loop(MinTimeout, NewMaxTimeout, TimerRef, Term, ResponsePid);

        startSignal -> 
            % no checks on TimerRef since when we call startTimer, it is always undefined
            Timeout = randomTimeout(MinTimeout, MaxTimeout),
            NewTimerRef = erlang:send_after(Timeout, self(), electionTimeout),
            loop(MinTimeout, MaxTimeout, NewTimerRef, Term, ResponsePid);       

        {resetSignal, TermReq} -> 
            case TermReq >= Term of 
                true ->
                    case TimerRef =/= undefined of 
                        true ->     
                            erlang:cancel_timer(TimerRef),
                            flushTimeout(); % clear the timer (if it ran out)
                        _ -> 
                            ok
                    end,
                    Timeout = randomTimeout(MinTimeout, MaxTimeout),
                    NewTimerRef = erlang:send_after(Timeout, self(), electionTimeout);
                false -> % stale request, ignore it
                    NewTimerRef = TimerRef
            end,
            loop(MinTimeout, MaxTimeout, NewTimerRef, erlang:max(TermReq, Term), ResponsePid);

        {stopSignal, TermReq} ->
            case TimerRef =/= undefined andalso TermReq >= Term of 
                true -> 
                    erlang:cancel_timer(TimerRef),
                    flushTimeout(), % clear the timer (if it ran out)
                    loop(MinTimeout, MaxTimeout, undefined, erlang:max(TermReq, Term), ResponsePid);
                false -> % stale request or no timer is currently running, ignore it
                    loop(MinTimeout, MaxTimeout, TimerRef, erlang:max(TermReq, Term), ResponsePid)
            end;

        electionTimeout -> 
            % provide the election timeout signal and election term
            ResponsePid ! {electionTimeoutSignal, Term+1},
            loop(MinTimeout, MaxTimeout, TimerRef, Term+1, ResponsePid)
    end.

% Helper function
randomTimeout(MinTimeout, MaxTimeout) ->
    MinTimeout + rand:uniform(MaxTimeout - MinTimeout).

flushTimeout() -> 
    receive 
        electionTimeout -> ok
    after 0 -> 
        ok
    end.