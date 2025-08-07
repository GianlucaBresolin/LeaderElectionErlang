-module(nodeSupervisor).
-export([start/0]).

start() ->
    process_flag(trap_exit, true),
    spawn_and_monitor().

spawn_and_monitor() ->
    NodeProcess = spawn(node, start, [self()]),

    link(NodeProcess, node),
    register(node, NodeProcess),

    receive 
        {'EXIT', NodeProcess, Reason} ->
            io:format("Node process exited with reason: ~p. Restarting...~n", [Reason]),
            unregister(node),
            spawn_and_monitor()
    end.