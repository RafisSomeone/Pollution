%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2019 21:47
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("rafal").

-behaviour(supervisor).


-export([start_link/0, init/1, stop/0]).

start_link() ->
  ets:new(backup, [set, public, named_table]),
  Monitor = pollution:createMonitor(),
  ets:insert(backup, [{lastState, Monitor}]),
  supervisor:start_link({local, pollutionSupervisor}, ?MODULE, []).

init(_InitValue) ->
  {ok, {
    {one_for_one, 3, 10},
    [ {gen_server,
      {pollution_gen_server, start, []},
      permanent, infinity, worker, [pollution_gen_server]}
    ]}
  }.

stop() ->
  supervisor:terminate_child(pollutionSupervisor, gen_server),
  supervisor:delete_child(pollutionSupervisor, gen_server),
  exit(self(), normal).