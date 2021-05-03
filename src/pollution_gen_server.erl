-module(pollution_gen_server).
-behaviour(gen_server).

-export([stop/0, addStation/2, addValue/4, removeValue/3, getMaximumOfType/1, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/2, numberOfMeasurements/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link(
    {local, pollution_gen_server},
    pollution_gen_server,
    0, []).

stop() ->
  gen_server:call(pollution_gen_server, terminate).

init(_) -> {ok, pollution:createMonitor()}.

%%crash() -> gen_server:call(pollution_gen_server, crash).

%% user interface

addStation(Name, Coordinates) ->
  gen_server:cast(pollution_gen_server, {addStation, Name, Coordinates}).

addValue(Name, Date, Type, Value) ->
  gen_server:cast(pollution_gen_server, {addValue, Name, Date, Type, Value}).

removeValue(Name, Date, Type) ->
  gen_server:cast(pollution_gen_server, {removeValue, Name, Date, Type}).

getOneValue(Name, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, Name, Date, Type}).

getStationMean(Name, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, Name, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Date, Type}).

getHourlyMean(Date, Type) ->
  gen_server:call(pollution_gen_server, {getHourlyMean, Date, Type}).

getMaximumOfType(Type) ->
  gen_server:call(pollution_gen_server, {getMaximumOfType, Type}).

numberOfMeasurements(Name, Type) ->
  gen_server:call(pollution_gen_server, {numberOfMeasurements, Name, Type}).

%% callbacks

handle_call({getOneValue, Name, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Monitor, Name, Date, Type), Monitor};
handle_call({getStationMean, Name, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Monitor, Name, Type), Monitor};
handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Monitor, Date, Type), Monitor};
handle_call({getHourlyMean, Date, Type}, _From, Monitor) ->
  {reply, pollution:getHourlyMean(Monitor, Date, Type), Monitor};
handle_call({getMaximumOfType, Type}, _From, Monitor) ->
  {reply, pollution:getMaximumOfType(Monitor, Type), Monitor};
handle_call({numberOfMeasurements, Name, Type}, _From, Monitor) ->
  {reply, pollution:numberOfMeasurements(Monitor, Name, Type), Monitor};
handle_call(terminate, _From, M) ->
  {stop, normal, ok, M}.

handle_cast({addStation, Name, Coordinates}, Monitor) ->
  case pollution:addStation(Monitor, Name, Coordinates) of
    {error, _} -> {noreply, Monitor};
    NewMonitor -> {noreply, NewMonitor}
  end;
handle_cast({addValue, Name, Date, Type, Value}, Monitor) ->
  case pollution:addValue(Monitor, Name, Date, Type, Value) of
    {error, _} -> {noreply, Monitor};
    NewMonitor -> {noreply, NewMonitor}
  end;
handle_cast({removeValue, Name, Date, Type}, Monitor) ->
  case pollution:removeValue(Monitor, Name, Date, Type) of
    {error, _} -> {noreply, Monitor};
    NewMonitor -> {noreply, NewMonitor}
  end.
