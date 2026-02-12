%%%-------------------------------------------------------------------
%%% @doc
%%% Floor 23 - Erlang Jurisdiction Application
%%% OTP Application callback module
%%% @end
%%%-------------------------------------------------------------------
-module(department_floor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    department_floor_sup:start_link().

stop(_State) ->
    ok.
