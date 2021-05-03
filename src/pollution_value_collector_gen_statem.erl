-module(pollution_value_collector_gen_statem).
-behaviour(gen_statem).
-export([start_link/0, init/1, callback_mode/0]).
-export([setStation/1, addValue/3, storeData/0]).
-export([idle/3, collect/3]).


start_link() ->
  gen_statem:start_link({local, pollution_value_collector_gen_statem}, ?MODULE, [], []).

init([]) -> {ok, idle, []}.
callback_mode() -> state_functions.


stop() -> gen_statem:stop(pollution_value_collector_gen_statem).

setStation(Name) -> gen_statem:cast(?MODULE, {setStation, Name}).
addValue(Date, Type, Value) -> gen_statem:cast(?MODULE, {addValue, Date, Type, Value}).
storeData() -> gen_statem:cast(?MODULE, flushData).

idle(_Event, {setStation, Name}, _None) ->
  {next_state, collect, #{name => Name, values => []}}.

collect(_Event, {addValue, Date, Type, Value}, #{values := Values} = Data) ->
  io:format("collecting data~n"),
  {next_state, collect, Data#{values => [{Date, Type, Value} | Values]}};
collect(_Event, flushData, #{values := Values, name := Name}) ->
  io:format("storing data~n"),
  lists:foreach(fun({Date, Type, Value}) -> pollution_gen_server:addValue(Name, Date, Type, Value) end, Values),
  {next_state, idle, []}.