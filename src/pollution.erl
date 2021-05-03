%%Author: Jakub Libera

-module(pollution).
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/3, getMaximumOfType/2, numberOfMeasurements/3]).

-record(monitor, {stations}).
-record(station, {name, coordinates, measurements}).
-record(measurement, {date, type, value}).

%%stations w monitorze i measurments w station to mapy  #{}

%%drzewo struktury
%%monitor -
%%  staation1 {}
%%    -measurement1
%%    -measurement2
%%    -measurement3
%%  staation2
%%    -measurement1
%%    -measurement2
%%    -measurement3
%%  staation3
%%    -measurement1
%%    -measurement2
%%    -measurement3


createMonitor() ->
  #monitor{stations = #{}}.

addStation(Monitor, Name, Coordinates) ->
  Temp1 = Monitor#monitor.stations,
  S = lists:flatlength(lists:filter(fun({_, X}) ->
    ((X#station.coordinates == Coordinates) or (X#station.name == Name)) end, maps:to_list(Temp1))),
  case S of
    1 -> {error, "Can't add Station, name or coordinates taken"};
    _ ->
      Monitor#monitor{stations = maps:put(Name, #station{name = Name, coordinates = Coordinates, measurements = #{}}, Temp1)}
  end.

addValue(Monitor, Name, Date, Type, Value) ->
  Temp1 = Monitor#monitor.stations,
  T = maps:get(Name, Temp1, nothing),
  case T of
    nothing ->
      T1 = lists:flatlength(lists:filter(fun({_, X}) -> ((X#station.coordinates == Name)) end, maps:to_list(Temp1))),
      case T1 of
        0 -> {error, "No such station!"}
      end;
    _ ->
      T1 = T,
      {TName, TCoordinates, TMeasurements} = {T1#station.name, T1#station.coordinates, T1#station.measurements},
      M = maps:get({Date, Type}, TMeasurements, nothing),
      case M of
        nothing -> Monitor#monitor{
          stations = maps:put(
            Name,
            #station{
              name = TName,
              coordinates = TCoordinates,
              measurements = TMeasurements#{
                {Date, Type} => #measurement{
                  date = Date,
                  type = Type,
                  value = Value}}},
            Temp1)};
        _ -> {error, "Cant add that value"}
      end
  end.

removeValue(Monitor, Name, Date, Type) ->
  Temp1 = Monitor#monitor.stations,
  Temp2 = maps:get(Name, Temp1),
  Monitor#monitor{
    stations = maps:put(
      Name,
      #station{
        name = Temp2#station.name,
        coordinates = Temp2#station.coordinates,
        measurements = maps:remove({Date, Type}, Temp2#station.measurements)},
      Temp1)}.

getOneValue(Monitor, Name, Date, Type) ->
  Temp = maps:get(Name, Monitor#monitor.stations),
  {_, _, _, Value} = maps:get({Date, Type}, Temp#station.measurements),
  Value.

getStationMean(Monitor, Name, Type) ->
  Temp = maps:get(Name, Monitor#monitor.stations),
  getStationMeanSupport(maps:to_list(Temp#station.measurements), Type, 0) / maps:size(Temp#station.measurements).

getStationMeanSupport([], _, Sum) -> Sum;
getStationMeanSupport([{{_, _}, {_, _, Type, Value}} | T], LookFor, Sum) ->
  case Type of
    LookFor -> getStationMeanSupport(T, LookFor, Sum + Value);
    _ -> getStationMeanSupport(T, LookFor, Sum)
  end.

getDateMean(Monitor, Date, Type, DateType) ->
  getDateMeanSupport(maps:to_list(Monitor#monitor.stations), Date, Type, 0, 0, DateType).

getDateMeanSupport([], _, _, Sum, D, _) ->
  case D of
    0 -> {error, "no messurments that day"};
    _ -> Sum / D
  end;
getDateMeanSupport([{_, Station} | T], Date, Type, Sum1, D1, DateType) ->
  Temp = lists:filter(
    fun({_, X}) -> L = getPreparedDate(DateType, X#measurement.date), {L, X#measurement.type} == {Date, Type} end,
    maps:to_list(Station#station.measurements)
  ),
  D2 = lists:flatlength(Temp),
  Sum2 = lists:foldl(
    fun({_, X}, Y) -> X#measurement.value + Y end,
    0,
    Temp
  ),
  getDateMeanSupport(T, Date, Type, Sum1 + Sum2, D1 + D2, DateType).

getPreparedDate(DateType, {Day, {Hour, _, _}}) ->
  case DateType of
    "Hour" -> Hour;
    "Day" -> Day
  end.

getDailyMean(Monitor, {Day, _}, Type) ->
  getDateMean(Monitor, Day, Type, "Day").

%%zadanie niespodzianka

getHourlyMean(Monitor, {_, {Hour, _, _}}, Type) ->
  getDateMean(Monitor, Hour, Type, "Hour").

%%własne zadania

%%funkcja zwraca najwiekszy pomiar danego typu
getMaximumOfType(Monitor, Type) ->
  lists:max([Z || {_, {_, _, T, Z}} <- lists:flatten([Y || Y <- [maps:to_list(X#station.measurements) || {_, X} <- maps:to_list(Monitor#monitor.stations)]]), T == Type]).

%%funkcja zwraca liczbe pomiarow danego typu ze stacji
numberOfMeasurements(Monitor, Name, Type) ->
  lists:flatlength([Z || {_, {_, _, T, Z}} <- lists:flatten([Y || Y <- [maps:to_list(X#station.measurements) || {_, X} <- maps:to_list(Monitor#monitor.stations), X#station.name == Name]]), T == Type]).
