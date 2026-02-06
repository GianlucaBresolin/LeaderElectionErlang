%%%-------------------------------------------------------------------
%% @doc leaderElectionErlang public API
%% @end
%%%-------------------------------------------------------------------

-module(leaderElectionNode_app).

-behaviour(application).

-export([start/2, start_node/2, stop/1]).

start(_StartType, _StartArgs) ->
    leaderElectionNode_sup:start_link().

stop(_State) ->
    ok.

% Internal function
start_node(MyID, ConfigurationList) ->
    timer:sleep(1000), % give some time for the servers to start before printing the initialization message
    % Initialize node's components
    TermPid = term:start(),
    StatePid = state:start(),
    MyVotePid = myVote:start(),
    CurrentLeaderPid = currentLeader:start(),
    ElectionTimerPid = electionTimer:startTimer(self()),
    VoteCountPid = voteCount:start(ConfigurationList),

    % Starts server for RPCs
    voteRequestRPC:startServer(TermPid, StatePid, ElectionTimerPid, MyVotePid), 
    heartbeatRequestRPC:startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid), 

    io:format("~p initialized ~n", [MyID]), 

    % call the main loop to handle elections and leaderships
    loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList).

% Internal loop
loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList) ->
    receive
        {electionTimeoutSignal, ElectionTerm} ->
            % Start the election process
            handleElection:handleElection(ElectionTerm, ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList, self()), 
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList);
        {becomeLeaderSignal, LeadershipTerm} ->
            % Start the leader process
            handleLeadership:handleLeadership(ElectionTimerPid, StatePid, TermPid, ConfigurationList, LeadershipTerm, MyID),
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList)
    end.