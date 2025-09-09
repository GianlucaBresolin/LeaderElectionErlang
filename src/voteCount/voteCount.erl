-module(voteCount).
-export([start/1, addVote/4]).

% API
start(ConfigurationList) ->
    VoterMap = maps:from_list([{VoterID, 0} || {VoterID} <- ConfigurationList]),
    spawn_link( fun() -> loop(0, 0, VoterMap, false) end ).

addVote(Pid, VoterID, Term, BecomeLeaderPid) ->
    Pid ! {addVote, VoterID, Term, BecomeLeaderPid},
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
                    {UpdatedVoteCount, UpdatedTerm, UpdatedVoterMap, UpdatedLeaderFlag} =
                        case TermReq > Term of
                            true ->
                                % reset the vote count and update the term and leader flag
                                {0, TermReq, resetVoterMap(VoterMap), false};
                            false ->  % TermReq == Term
                                {VoteCount, Term, VoterMap, LeaderFlag}
                        end,
                    case catch maps:get(VoterID, UpdatedVoterMap) of
                        {'EXIT', {badkey, _}} ->
                            exit({error, "Voter not found"});
                        VotedFlag ->
                            case VotedFlag of
                                1 ->
                                    % already voted, ignore
                                    loop(UpdatedVoteCount, UpdatedTerm, UpdatedVoterMap, LeaderFlag);
                                0 -> 
                                    io:format("NODE ~s GOT VOTE FROM ~s FOR TERM ~p~n", [VoterID, VoterID, UpdatedTerm]),
                                    NewVoteCount = UpdatedVoteCount + 1,
                                    FinalVoterMap = UpdatedVoterMap#{VoterID := 1},
                                    case NewVoteCount >= maps:size(UpdatedVoterMap) div 2 + 1 andalso not UpdatedLeaderFlag of 
                                        true ->
                                            % become leader
                                            BecomeLeaderPid ! {becomeLeaderSignal, UpdatedTerm},
                                            loop(NewVoteCount, UpdatedTerm, FinalVoterMap, true);
                                        false ->
                                            loop(NewVoteCount, UpdatedTerm, FinalVoterMap, UpdatedLeaderFlag)
                                    end
                            end
                    end
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
