-module(handleElection).
-export([handleElection/8]).

% API

handleElection(ElectionTimerPid, TermPid, StatePid, VoteCountPid, MyVotePid, VoteCountID, MyID, BecomeLeaderPid) ->
    % reset the election timer to resolve split-votes
    electionTimer:reset(ElectionTimerPid),

    % increment the current term
    term:inc(TermPid, self()),
    receive 
        {term, NewTerm} -> NewTerm
    end,

    % become a candidate 
    state:setCandidate(StatePid, NewTerm),

    % reset the vote count
    voteCount:reset(VoteCountPid, NewTerm),

    % set my vote for self
    case SuccessMyVote = myVote:setMyVote(MyVotePid, MyID, NewTerm) of
        false ->
            state:setFollower(StatePid, none, ElectionTimerPid, none, NewTerm),
        _ ->
            ok
    end,

    % add my vote to the vote count
    voteCount:addVote(VoteCountPid, MyID, NewTerm, BecomeLeaderPid),

    % send vote requests to all other nodes
    %TODO

% Internal stuff
askVote() ->
    %TODO