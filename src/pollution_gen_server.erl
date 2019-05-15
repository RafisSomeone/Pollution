%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2019 09:55
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("rafal").
-behavior(gen_server).

%% API
-export([start/0,init/1, handle_call/3, handle_cast/2]).
-export([addStation/2,addValue/4,removeValue/3,stop/0,getNorms/0,getOneValue/3,getOverLimit/3,getDailyMean/2,getStationMean/2,print/0,crash/0,terminate/2]).
start() ->
  [{lastState, State}] = ets:lookup(backup, lastState),
  gen_server:start_link({local,pollution_gen_server},?MODULE,State,[]).

init(Monitor) ->
  {ok, Monitor}.

process(noreply,{error,Text},Monitor) ->
  io:format("error: ~s~n", [Text]),
  {noreply,Monitor};
process(noreply,Result,_) ->
  {noreply,Result};
process(reply,{error,Text},Monitor) ->
  io:format("error: ~s~n", [Text]),
  {reply,0,Monitor};
process(reply,Result,Monitor) ->
  {reply,Result,Monitor}.

addStation(Name, Location) ->
  gen_server:cast(?MODULE,{addStation,Name,Location}).

addValue(Searched,Date,Type,Value) ->
  gen_server:cast(?MODULE,{addValue,Searched,Date,Type,Value}).

removeValue(Searched,Date,Type) ->
  gen_server:cast(?MODULE,{removeValue,Searched,Date,Type}).
stop() ->
  gen_server: cast(?MODULE, stop).

getNorms() ->
  gen_server:call(?MODULE,getNorms).

getOneValue(Searched,Type,Date)->
  gen_server:call(?MODULE,{getOneValue,Searched,Type,Date}).

getStationMean(Searched,Type)->
  gen_server:call(?MODULE,{getStationMean,Searched,Type}).

getDailyMean(Date,Type)->
  gen_server:call(?MODULE,{getDailyMean,Date,Type}).

getOverLimit(Hour,Type,Norms)->
  gen_server:call(?MODULE,{getOverLimit,Hour,Type,Norms}).
print() ->
  gen_server:call(?MODULE,print).
crash() ->
  gen_server:cast(?MODULE,crash).

terminate(Reason, Monitor) ->
  ets:insert(backup, [{lastState, Monitor}]),
  io:format("Data saved~n"),
  Reason.


handle_call({getNorms},_From, Monitor) ->
  Result = pollution:createNorms(),
  process(reply,Result,Monitor);

handle_call({getOneValue,Searched,Type,Date},_From, Monitor) ->
  Result = pollution:getOneValue(Monitor,Searched,Type,Date),
  process(reply,Result,Monitor);

handle_call({getStationMean,Searched,Type},_From, Monitor) ->
  Result = pollution:getStationMean(Monitor,Searched,Type),
  process(reply,Result,Monitor);

handle_call({getDailyMean,Date,Type},_From, Monitor) ->
  Result = pollution:getStationMean(Monitor,Date,Type),
  process(reply,Result,Monitor);

handle_call({getOverLimit,Hour,Type,Norms},_From, Monitor) ->
  Result = pollution:getOverLimit(Monitor,Hour,Type,Norms),
  process(reply,Result,Monitor);

handle_call(print,_From,Monitor) ->
  process(reply,Monitor,Monitor).

handle_cast({crash},Monitor) ->
  Result = 1/0,
  process(noreplay,Result,Monitor);

handle_cast({addStation,Name,Location}, Monitor) ->
  Result = pollution:addStation(Name, Location,Monitor),
  process(noreply,Result,Monitor);

handle_cast({addValue,Searched,Date,Type,Value}, Monitor) ->
  Result = pollution:addValue(Searched,Date,Type,Value,Monitor),
  process(noreply,Result,Monitor);

handle_cast({removeValue,Searched,Date,Type}, Monitor) ->
  Result = pollution:removeValue(Monitor,Searched,Date,Type),
  process(noreply,Result,Monitor);

handle_cast(stop, Monitor) ->
  {stop, normal, Monitor}.
