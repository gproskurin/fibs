%%%-------------------------------------------------------------------
%% @doc fibs public API
%% @end
%%%-------------------------------------------------------------------

-module(fibs_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/fibs/generate">>, fibs_http_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        fibs_http_listener,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ),
    fibs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
