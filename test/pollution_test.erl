%%Author: Jakub Libera

-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


pollution_createMonitor_test() ->
  {monitor, #{}} =
    pollution:createMonitor().
pollution_addStation_test() ->
  {monitor, #{"testName" :=
  {station, "testName", {50.2345, 18.3445}, #{}},
    "testName2" := {station, "testName2", {1, 1}, #{}}}} =
    pollution:addStation(
      pollution:addStation(
        pollution:createMonitor(), "testName", {50.2345, 18.3445}),
      "testName2", {1, 1}).
pollution_addValue_test() ->

  {monitor,
    #{"testName" :=
    {station, "testName",
      {50.2345, 18.3445},
      #{{"time", "PM10"} :=
      {measurement, "time", "PM10", 59},
        {"time", "PM5"} :=
        {measurement, "time", "PM5", 65}}}}} =
    pollution:addValue(
      pollution:addValue(
        pollution:addStation(
          pollution:createMonitor(), "testName", {50.2345, 18.3445}),
        "testName", "time", "PM10", 59),
      "testName", "time", "PM5", 65).
pollution_removeValue_test() ->
  {monitor, #{"testName" :=
  {station, "testName", {50.2345, 18.3445}, #{}}}} =
    pollution:removeValue(
      pollution:addValue(
        pollution:addStation(
          pollution:createMonitor(), "testName", {50.2345, 18.3445}),
        "testName", "time", "PM10", 59),
      "testName", "time", "PM10").
pollution_getOneValue_test() ->
  59 =
    pollution:getOneValue(
      pollution:addValue(
        pollution:addValue(
          pollution:addStation(
            pollution:createMonitor(), "testName", {50.2345, 18.3445}),
          "testName", "time", "PM10", 59),
        "testName", "time", "PM5", 65),
      "testName", "time", "PM10").
pollution_getStationMean_test() ->
  2.0 =
    pollution:getStationMean(
      pollution:addValue(
        pollution:addValue(
          pollution:addStation(
            pollution:createMonitor(), "testName", {50.2345, 18.3445}),
          "testName", "time1", "PM10", 1),
        "testName", "time2", "PM10", 3),
      "testName", "PM10").
pollution_getDailyMean_test() ->
  2.0 =
    pollution:getDailyMean(
      pollution:addValue(
        pollution:addValue(
          pollution:addStation(
            pollution:createMonitor(), "testName", {50.2345, 18.3445}),
          "testName", {{2021, 4, 5}, {18, 12, 54}}, "PM10", 1),
        "testName", {{2021, 4, 5}, {18, 12, 53}}, "PM10", 3),
      {{2021, 4, 5}, {18, 12, 53}}, "PM10").
pollution_getHourlyMean_test() ->
  2.0 =
    pollution:getHourlyMean(
      pollution:addValue(
        pollution:addValue(
          pollution:addStation(
            pollution:createMonitor(), "testName", {50.2345, 18.3445}),
          "testName", {{2021, 4, 5}, {18, 12, 54}}, "PM10", 1),
        "testName", {{2021, 4, 5}, {18, 12, 53}}, "PM10", 3),
      {{2021, 4, 5}, {18, 12, 53}}, "PM10").
pollution_getMaximumOfType_test() ->
  8 =
    pollution:getMaximumOfType(
      pollution:addValue(
        pollution:addStation(
          pollution:addValue(
            pollution:addValue(
              pollution:addStation(
                pollution:createMonitor(), "testName", {50.2345, 18.3445}),
              "testName", {"t", 1}, "PM10", 1),
            "testName", {"t", 2}, "PM5", 9),
          "testName1", {50.23245, 19.3445}),
        "testName1", {"t", 2}, "PM10", 8),
      "PM10").

pollution_numberOfMeasurements_test() ->
  1 =
    pollution:numberOfMeasurements(
      pollution:addValue(
        pollution:addStation(
          pollution:addValue(
            pollution:addValue(
              pollution:addStation(
                pollution:createMonitor(), "testName", {50.2345, 18.3445}),
              "testName", {"t", 1}, "PM10", 1),
            "testName", {"t", 2}, "PM5", 9),
          "testName1", {50.2345, 19.3445}),
        "testName1", {"t", 2}, "PM10", 8),
      "testName", "PM10").