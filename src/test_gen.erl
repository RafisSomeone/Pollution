%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. kwi 2019 00:02
%%%-------------------------------------------------------------------
-module(test_gen).
-include_lib("eunit/include/eunit.hrl").
-author("rafal").
-compile(export_all).

%% API
-export([]).

-record(station, {name, position, measurements = []}).
-record(measurement, {type, value, date}).
-record(norm,{name,value}).


addStation_test() ->
  pollution_gen_server:start(),
  ?assertEqual(pollution_gen_server:addStation("Londyn", {45.23, 98.12}), ok),
  pollution_gen_server:stop().

addValueName_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  ?assertEqual(pollution_gen_server:addValue("Brooklyn", calendar:local_time(), "PM10", 43.0), ok),
  pollution_gen_server:stop().

addValueLocation_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  ?assertEqual(pollution_gen_server:addValue({123.5, 532.3}, calendar:local_time(), "PM10", 43.0), ok),
  pollution_gen_server:stop().

removeValueName_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  pollution_gen_server:addValue("Brooklyn", {{2019,4,24}, {14,2,35}}, "PM10", 12.0),
  ?assertEqual(pollution_gen_server:removeValue("Brooklyn",  {{2019,4,24}, {14,2,35}}, "PM10"), ok),
  pollution_gen_server:stop().


removeValueLocation_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  pollution_gen_server:addValue("Brooklyn", {{2019,4,24}, {14,2,35}}, "PM10", 12.0),
  ?assertEqual(pollution_gen_server:removeValue({123.5, 532.3},  {{2019,4,24}, {14,2,35}}, "PM10"), ok),
  pollution_gen_server:stop().

getOneValueName_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  pollution_gen_server:addValue("Brooklyn", {{2019,4,24}, {14,2,35}}, "PM10", 12.0),
  ?assertEqual(pollution_gen_server:getOneValue("Brooklyn","PM10" ,{{2019,4,24}, {14,2,35}}), 12.0),
  pollution_gen_server:stop().

getOneValueLocation_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  pollution_gen_server:addValue("Brooklyn", {{2019,4,24}, {14,2,35}}, "PM10", 12.0),
  ?assertEqual(pollution_gen_server:getOneValue({123.5, 532.3},"PM10" ,{{2019,4,24}, {14,2,35}}), 12.0),
  pollution_gen_server:stop().

getOverLimit_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Brooklyn", {123.5, 532.3}),
  pollution_gen_server:addValue("Brooklyn", {{2019,4,24}, {14,2,35}}, "PM10", 150.0),
  pollution_gen_server:addValue("Brooklyn", {{2019,3,24}, {14,28,35}}, "PM10", 12.0),
  pollution_gen_server:addStation("Brooklyn1", {123.5, 552.3}),
  pollution_gen_server:addValue("Brooklyn1", {{2019,4,24}, {14,31,35}}, "PM10", 34.0),
  pollution_gen_server:addValue("Brooklyn1", {{2018,4,24}, {14,12,35}}, "PM10", 121.0),
  pollution_gen_server:addStation("Brooklyn2", {123.5, 572.3}),
  pollution_gen_server:addValue("Brooklyn2", {{2019,4,24}, {14,2,35}}, "PM10", 150.0),
  pollution_gen_server:addValue("Brooklyn2", {{2019,4,24}, {14,25,35}}, "PM10", 12.0),
  ?assertEqual(pollution_gen_server:getOverLimit({{2019,4,24}, {14,2,35}}, "PM10", pollution_gen_server:getNorms()), 2),
  pollution_gen_server:stop().
