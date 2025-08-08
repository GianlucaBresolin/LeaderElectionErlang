-module(node).
-export([start/1]).

% API
start(MyID, ConfigurationList) ->
    spawn(
        fun() ->
            init(MyID, ConfigurationList)
        end).

% Internal initialization
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
            handleElection:handleElection(ElectionTerm, ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList)
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList);
        {becomeLeaderSignal, LeadershipTerm} ->
            % Start the leader process
            handleLeadership:handleLeadership(ElectionTimerPid, StatePid, ConfigurationList, LeadershipTerm, MyID),
            loop(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, MyID, ConfigurationList);
    end.
