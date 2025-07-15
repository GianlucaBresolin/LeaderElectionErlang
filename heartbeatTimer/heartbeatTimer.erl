-module(heartbeatTimer).
-export([startTimer/1, resetTimer/1, stopTimer/1, setTimeout/2]).

% API
startTimer(ResponsePid) ->  
    Pid = spawn(fun() -> loop(10, undefined, ResponsePid) end), % default timeout = 10 ms
    Pid ! startSignal,
    Pid.
    
resetTimer(Pid) -> 
    Pid ! resetSignal,
    ok.

stopTimer(Pid) ->
    Pid ! stopSignal,
    ok.

setTimeout(Pid, NewTimeout) ->
    Pid ! {setTimeout, NewTimeout},
    ok.

% Internal loop
loop(Timeout, TimerRef, ResponsePid) ->
    receive 
        {setTimeout, NewTimeout} ->
            loop(NewTimeout, TimerRef, ResponsePid);

        startSignal -> 
            % no checks on TimerRef since when we call startTimer, it is always undefined
            NewTimerRef = erlang:send_after(Timeout, self(), heartbeatTimeout),
            loop(Timeout, NewTimerRef, ResponsePid);           
        resetSignal -> 
            case TimerRef =/= undefined of 
                true ->     
                    erlang:cancel_timer(TimerRef),
                    flushTimeout(), % clear the timer (if it ran out)
                    NewTimerRef = erlang:send_after(Timeout, self(), heartbeatTimeout),
                    loop(Timeout, NewTimerRef, ResponsePid);
                false ->
                    exit({error, "Timer not started"})
            end;
        stopSignal ->
            case TimerRef =/= undefined of 
                true -> 
                    erlang:cancel_timer(TimerRef),
                    flushTimeout(), % clear the timer (if it ran out)
                    loop(Timeout, undefined, ResponsePid);
                false -> 
                    loop(Timeout, undefined, ResponsePid)
            end;

        heartbeatTimeout -> 
            ResponsePid ! {heartbeatTimeoutSignal},
            loop(Timeout, TimerRef, ResponsePid)
    end.

% Helper function
flushTimeout() -> 
    receive 
        heartbeatTimeout -> ok
    after 0 -> 
        ok
    end.