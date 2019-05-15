%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2019 10:16
%%%-------------------------------------------------------------------
-module(pollution_server_subb).
-author("rafal").

%% API
-export([start/0,stop/0]).

start() ->  register(sup, spawn(fun() -> loop() end )).

stop() -> sup ! stop.

loop() ->
  process_flag(trap_exit,true),
  io:format("starting server ~n"),
  pollution_server:start(),
  receive
    {'EXIT',Pid,Reason} ->io:format("crush ~p~n",[Reason]) ,loop();
    stop -> pollution_server:stop(),ok
  end .