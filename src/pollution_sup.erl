%%%-------------------------------------------------------------------
%% @doc pollution top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-version('1.0').
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, pollutionSupervisor}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 2,
    period => 5},
  ServerSpec = #{id => 'pollution_gen_server',
    start => {pollution_gen_server, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [pollution_gen_server]},
  StatemSpec = #{id => 'pollution_value_collector_gen_statem',
    start => {pollution_value_collector_gen_statem, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [pollution_value_collector_gen_statem]},
  {ok, {SupFlags, [ServerSpec, StatemSpec]}}.