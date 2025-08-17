-module(voteCount).
-export([start/1, addVote/4, reset/2]).

% API
start(ConfigurationList) ->
    VoterList = lists:map(fun(VoterID) -> {VoterID, 0} end, ConfigurationList),
    VoterMap = maps:from_list(VoterList),
    spawn_link( fun() -> loop(0, 0, VoterMap, false) end ).

addVote(Pid, VoterID, Term, BecomeLeaderPid) ->
    Pid ! {addVote, VoterID, Term, BecomeLeaderPid},
    ok.

reset(Pid, Term) ->
    Pid ! {reset, Term},
    ok.

% Internal loop
loop(VoteCount, Term, VoterMap, LeaderFlag) ->
    receive 
        {addVote, VoterID, TermReq, BecomeLeaderPid} ->
            case TermReq < Term of 
                true -> 
                    % stale request, ignore it
                    loop(VoteCount, Term, VoterMap, LeaderFlag);
                false ->
                    {UpdatedTerm, UpdatedVoterMap, UpdatedLeaderFlag} =
                        case TermReq > Term of
                            true ->
                                % reset the vote count and update the term and leader flag
                                {TermReq, resetVoterMap(VoterMap), false};
                            false ->  % TermReq == Term
                                {Term, VoterMap, LeaderFlag}
                        end,
                    case catch maps:get(VoterID, UpdatedVoterMap) of
                        {'EXIT', {badkey, _}} ->
                            exit({error, "Voter not found"});
                        VotedFlag ->
                            case VotedFlag of
                                1 ->
                                    % already voted, ignore
                                    loop(VoteCount, UpdatedTerm, UpdatedVoterMap, LeaderFlag);
                                0 -> 
                                    UpdatedVoteCount = VoteCount + 1,
                                    FinalVoterMap = UpdatedVoterMap#{VoterID := 1},
                                    case UpdatedVoteCount >= maps:size(UpdatedVoterMap) div 2 + 1 andalso not UpdatedLeaderFlag of 
                                        true ->
                                            % become leader
                                            BecomeLeaderPid ! {becomeLeaderSignal, UpdatedTerm},
                                            loop(UpdatedVoteCount, UpdatedTerm, FinalVoterMap, true);
                                        false ->
                                            loop(UpdatedVoteCount, UpdatedTerm, FinalVoterMap, UpdatedLeaderFlag)
                                    end
                            end
                    end
            end;
                                
        {reset, NewTerm} ->
            case NewTerm > Term of
                true ->
                    UpdatedVoterMap = resetVoterMap(VoterMap),
                    loop(0, NewTerm, UpdatedVoterMap, false);
                false ->
                    % the reset for that term has already been done, ignore
                    loop(VoteCount, Term, VoterMap, LeaderFlag)
            end
    end.

% Helper function
resetVoterMap(VoterMap) ->
    maps:fold( 
        fun(Key, _, AccMap) ->
            maps:put(Key, 0, AccMap)
        end,
        #{}, 
        VoterMap).
