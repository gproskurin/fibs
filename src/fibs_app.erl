%%%-------------------------------------------------------------------
%% @doc fibs public API
%% @end
%%%-------------------------------------------------------------------

-module(fibs_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fibs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
