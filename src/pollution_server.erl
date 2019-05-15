%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2019 22:58
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("rafal").
%% API
-export([start/0, stop/0,crash/0, init/0,addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getOverLimit/3, print/0,getNorms/0]).


start() ->
  register(pollutionServer, spawn_link(pollution_server, init, [])).

init() ->
  loop(pollution:createMonitor()).

stop() ->
  pollutionServer ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.

loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, Location}} ->
      Result = pollution:addStation(Name, Location,Monitor),
      process(Result, Pid, Monitor);

    {request, Pid, getNorms,_} ->
      Pid ! {reply, pollution:createNorms()},
      loop(Monitor);

    {request, Pid, addValue, {Searched,Date,Type,Value}} ->
      Result = pollution:addValue(Searched,Date,Type,Value,Monitor),
      process(Result, Pid, Monitor);

    {request, Pid, removeValue, {Searched,Date,Type}} ->
      Result = pollution:removeValue(Monitor,Searched,Date,Type),
      process(Result, Pid, Monitor);

    {request, Pid, getOneValue, {Searched,Type,Date}} ->
      Pid ! {reply, pollution:getOneValue(Monitor,Searched,Type,Date)},
      loop(Monitor);


    {request, Pid, getStationMean, {Searched,Type}} ->
      Pid ! {reply, pollution:getStationMean(Monitor,Searched,Type)},
      loop(Monitor);

    {request, Pid, getDailyMean, {Date,Type}} ->
      Pid ! {reply, pollution:getDailyMean(Monitor,Date,Type)},
      loop(Monitor);

    {request, Pid, getOverLimit, {Hour,Type,Norms}} ->
      Pid ! {reply, pollution:getOverLimit(Monitor,Hour,Type,Norms)},
      loop(Monitor);

    {request, Pid, print} ->
      Pid ! {reply, Monitor},
      loop(Monitor);

    {request, Pid, stop} ->
      Pid ! {reply, ok};
    {request, Pid ,crash} ->
      Pid ! {reply,1/0},
      loop(Monitor)
  end.

process({error, Text}, Pid, Monitor) ->
  Pid ! {reply, Text},
  loop(Monitor);
process(Result, Pid, _) ->
  Pid ! {reply, ok},
  loop(Result).

call(Command, Args) ->
  pollutionServer ! {request, self(), Command, Args},
  receive
    {reply, Reply} -> Reply
  end.


print() ->
  pollutionServer ! {request, self(), print},
  receive
    {reply, Reply} -> Reply
  end.



addStation(Name, Location) -> call(addStation, {Name, Location}).
addValue(Searched,Date,Type,Value) -> call(addValue, {Searched,Date,Type,Value}).
removeValue(Searched,Date,Type) -> call(removeValue, {Searched,Date,Type}).
getOneValue(Searched,Type,Date) -> call(getOneValue, {Searched,Type,Date}).
getStationMean(Searched,Type) -> call(getStationMean, {Searched,Type}).
getDailyMean(Date,Type) -> call(getDailyMean, {Date,Type}).
getOverLimit(Hour,Type,Norms) -> call(getOverLimit, {Hour,Type,Norms}).
getNorms() -> call(getNorms,0).
crash() -> pollutionServer ! {request,self(),crash}.
