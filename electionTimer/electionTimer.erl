-module(electionTimer).
-export([startTimer/1, resetTimer/1, stopTimer/1, setMinTimeout/2, setMaxTimeout/2]).

% API
startTimer(ResponsePid) ->  
    Pid = spawn(fun() -> loop(150, 300, undefined, ResponsePid) end), % default min = 150, max = 300
    Pid ! startSignal,
    Pid.
    
resetTimer(Pid) -> 
    Pid ! resetSignal,
    ok.

stopTimer(Pid) ->
    Pid ! stopSignal,
    ok.

setMinTimeout(Pid, MinTimeout) ->
    Pid ! {minTimeout, MinTimeout},
    ok.

setMaxTimeout(Pid, MaxTimeout) ->
    Pid ! {maxTimeout, MaxTimeout},
    ok.


% Internal loop
loop(MinTimeout, MaxTimeout, TimerRef, ResponsePid) ->
    receive 
        {minTimeout, NewMinTimeout} ->
            loop(NewMinTimeout, MaxTimeout, TimerRef, ResponsePid);
        {maxTimeout, NewMaxTimeout} ->
            loop(MinTimeout, NewMaxTimeout, TimerRef, ResponsePid);

        startSignal -> 
            case TimerRef == undefined of 
                true -> 
                    Timeout = randomTimeout(MinTimeout, MaxTimeout),
                    NewTimerRef = erlang:send_after(Timeout, self(), electionTimeout),
                    loop(MinTimeout, MaxTimeout, NewTimerRef, ResponsePid);
                false -> 
                    exit({error, "Timer already started"})
            end;                
        resetSignal -> 
            case TimerRef =/= undefined of 
                true ->     
                    erlang:cancel_timer(TimerRef),
                    flushTimeout(), % clear the timer (if it ran out)
                    Timeout = randomTimeout(MinTimeout, MaxTimeout),
                    NewTimerRef = erlang:send_after(Timeout, self(), electionTimeout),
                    loop(MinTimeout, MaxTimeout, NewTimerRef, ResponsePid);
                false ->
                    exit({error, "Timer not started"})
            end;
        stopSignal ->
            case TimerRef =/= undefined of 
                true -> 
                    erlang:cancel_timer(TimerRef),
                    flushTimeout(), % clear the timer (if it ran out)
                    loop(MinTimeout, MaxTimeout, undefined, ResponsePid);
                false -> 
                    loop(MinTimeout, MaxTimeout, undefined, ResponsePid)
            end;

        electionTimeout -> 
            ResponsePid ! {electionTimeoutSignal},
            loop(MinTimeout, MaxTimeout, TimerRef, ResponsePid)
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