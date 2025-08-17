leaderElectionErlang
=====

A leader election implementation among a cluster of nodes in Erlang. 

Concurrency is achieved through actors. 

The leader election model is based on the *Raft Consensus Algorithm* ([Raft paper](https://raft.github.io/raft.pdf)).

Build
-----

    $ rebar3 compile

![Erlang](https://img.shields.io/badge/Erlang-28.0-blue)
