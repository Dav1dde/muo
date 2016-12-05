-module(muo_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  muo_main:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  muo_main:stop().
