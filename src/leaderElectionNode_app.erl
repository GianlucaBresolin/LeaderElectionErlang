%%%-------------------------------------------------------------------
%% @doc leaderElectionErlang public API
%% @end
%%%-------------------------------------------------------------------

-module(leaderElectionNode_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    leaderElectionNode_sup:start_link().

stop(_State) ->
    ok.

% Internal function
init(MyID, ConfigurationList) ->
    % Initialize node's components
    {ok, TermPid} = term:start(),
    {ok, StatePid} = state:start(),
    {ok, MyVotePid} = myVote:start(),
    {ok, CurrentLeaderPid} = currentLeader:start(),
    {ok, ElectionTimerPid} = electionTimer:startTimer(self()),
    {ok, VoteCountPid} = voteCount:start(ConfigurationList),

    % Starts server for RPCs
    voteRequestRPC:startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid),
    heartbeatRequestRPC:startServer(TermPid, StatePid, CurrentLeaderPid, MyVotePid, ElectionTimerPid),

    % call the main loop to handle elections and leaderships
    loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList).

% Internal loop
loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList) ->
    receive
        {electionTimeoutSignal, ElectionTerm} ->
            % Start the election process
            handleElection:handleElection(ElectionTerm, ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList),
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList);
        {becomeLeaderSignal, LeadershipTerm} ->
            % Start the leader process
            handleLeadership:handleLeadership(ElectionTimerPid, StatePid, ConfigurationList, LeadershipTerm, MyID),
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList)
    end.