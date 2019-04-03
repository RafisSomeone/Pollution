%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. kwi 2019 23:02
%%%-------------------------------------------------------------------
-module(pollution).
-author("rafal").

%% API
-export([createMonitor/0,createNorms/0, addStation/3, test/0, addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getOverLimit/4]).

-record(station, {name, position, measurements = []}).
-record(measurement, {type, value, date}).
-record(norm,{name,value}).

createMonitor() ->
  #{}.

createNorms() ->
  #{ "PM10" => #norm{name = "PM10", value = 50}, "PM2.5" => #norm{name = "PM2.5", value = 30}}.


sameLocation(Monitor, Location) ->

  lists:any(fun(Station) -> Station#station.position == Location end, maps:values(Monitor)).


addStation(Monitor, Name, Location) ->
  case maps:is_key(Name, Monitor) or sameLocation(Monitor, Location) of
    true -> {error, "There is already another station with these arguments"};
    _ ->
      Station = #station{name = Name, position = Location},
      Monitor#{Name => Station}
  end.

isStation(Monitor, {X, Y}) ->
  Station_List = lists:filter(fun(Station) -> Station#station.position == {X, Y} end, maps:values(Monitor)),
  case Station_List of
    [] -> {error, "There is no station with this position"};
    [Station] -> Station
  end;

isStation(Monitor, Name) ->
  Station_List = lists:filter(fun(Station) -> Station#station.name == Name end, maps:values(Monitor)),
    case Station_List of
      [] -> {error, "There is no station with this name"};
      [Station] -> Station
    end.
addValue(Monitor,Searched,Date,Type,Value) ->
  case isStation(Monitor,Searched) of
    {error,Text} -> {error,Text};
    Station -> saveValue(Monitor,Station#station.name,Date,Type,Value,Station)
  end.

saveValue(Monitor,Name,Date,Type,Value,Station)->
  Measurements = [#measurement{type = Type, date = Date, value = Value} | Station#station.measurements],
  NewStation = Station#station{measurements = Measurements},
  Monitor#{Name := NewStation}.

removeValue(Monitor,Searched,Date,Type)->
  case isStation(Monitor,Searched) of
    {error,Text} -> {error,Text};
    Station -> deleteValue(Monitor,Station#station.name,Date,Type,Station)
  end.
deleteValue(Monitor,Name,Date,Type,Station)->
  ToDelete_List=[ X || X <-Station#station.measurements, X#measurement.date==Date, X#measurement.type==Type],
  case ToDelete_List of
    [] -> {error,"No measurements match"};
    [ToDelete] -> Measurements = lists:delete(ToDelete,Station#station.measurements),
                  NewStation = Station#station{measurements = Measurements},
                  Monitor#{Name := NewStation}

  end.

getOneValue(Monitor,Searched,Type,Date)->
  case isStation(Monitor,Searched) of
    {error,Text} -> {error,Text};
    Station -> getOne(Date,Type,Station)
  end.
getOne(Date,Type,Station)->
  Find_List=[ X || X <-Station#station.measurements, X#measurement.date==Date, X#measurement.type==Type],
  case Find_List of
    [] -> {error,"No measurements match"};
    [ToFind] -> ToFind#measurement.value
  end.

getStationMean(Monitor,Searched,Type) ->
  case isStation(Monitor,Searched) of
    {error,Text} -> {error,Text};
    Station -> getMean(Station,Type)
  end .

getMean(Station,Type) ->
  ValueMeasurementList = [X#measurement.value || X <- Station#station.measurements,X#measurement.type==Type],
  lists:sum(ValueMeasurementList)/length(ValueMeasurementList).

getDailyMean(Monitor,Date,Type)->
  AllMeasurements = lists:merge([ X#station.measurements || X <- maps:values(Monitor) ]),
  SearchedValues=[ X#measurement.value || X <- AllMeasurements , (X#measurement.type==Type) and (sameDay(X#measurement.date,Date))],
  lists:sum(SearchedValues)/length(SearchedValues).

getOverLimit(Monitor,Hour,Type,Norms) ->
  Norm = maps:get(Type,Norms),
  IsOverList = [lists:any(fun(M) -> (M#measurement.type==Type) and (sameHour(M#measurement.date,Hour)) and (M#measurement.value>Norm#norm.value)  end ,X#station.measurements) || X <- maps:values(Monitor)],
  OverList = lists:map(fun(true) -> 1; (_) -> 0 end, IsOverList),
  lists:sum(OverList).

sameDay({{Year1, Month1,Day1},{_,_,_}},{{Year2,Month2,Day2},{_,_,_}}) ->
  (Year1==Year2) and (Month1==Month2) and (Day1==Day2).

sameHour({{Year1, Month1,Day1},{H1,_,_}},{{Year2,Month2,Day2},{H2,_,_}}) ->
  (Year1==Year2) and (Month1==Month2) and (Day1==Day2) and (H1==H2).






test() ->
  Station1 = #station{name = "Paryż", position = {4, 6}},
  Station2 = #station{name = "Londyn", position = {2, 3}},
  Station3 = #station{name = "Bytom", position = {14, 26}},
  Monitor = #{"Paryż" => Station1, "Londyn" => Station2, "Bytom" => Station3},

  NewMonitor = addValue(Monitor, {14, 26}, calendar:local_time(),"PM10", 122),
  NewMonitor2 = addValue(NewMonitor, {14, 26}, calendar:local_time(),"PM10", 112),
  NewMonitor3 = addValue(NewMonitor2, {14, 26}, calendar:local_time(),"PM2.5", 11),
  NewMonitor4 = addValue(NewMonitor3, "Londyn", calendar:local_time(),"PM10", 113),
  %%removeValue(NewMonitor2,{14,26},calendar:local_time(),tp).
  %%getOneValue(NewMonitor3,"Bytom",tp,calendar:local_time()),
  %%getStationMean(NewMonitor3,{14,26},ip).
  Type ="PM10",
  Data = {{2019,04,03},{21,29,50}},
  Norma =10,
  Hour = {{2019,04,03},{23,12,12}},
  Norms = createNorms(),
  Norm = maps:get("PM10",Norms),
  getOverLimit(NewMonitor4,Hour,"PM10",Norms).
