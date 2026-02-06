%%%-------------------------------------------------------------------
%% @doc leaderElectionErlang top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(leaderElectionNode_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 1
    },
    [NodeId | _] = string:tokens(atom_to_list(node()), "@"),
    {ok, ClusterNodes} = application:get_env(leaderElectionNode, cluster_nodes),
    ChildSpecs = [
        #{id => node,
          start => {leaderElectionNode_app, start_node, [NodeId, ClusterNodes]},
          restart => permanent,
          shutdown => infinity,
          type => worker,
          modules => [leaderElectionNode_app]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
