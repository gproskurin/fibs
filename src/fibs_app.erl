%%%-------------------------------------------------------------------
%% @doc fibs public API
%% @end
%%%-------------------------------------------------------------------

-module(fibs_app).

-behaviour(application).

-export([start/2, stop/1]).

-ifdef(TEST).
-export([blacklist_table_create/0, blacklist_table_destroy/0]).
-endif.

-include("include/fibs.hrl").


start(_StartType, _StartArgs) ->
    blacklist_table_create(),
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
    blacklist_table_destroy(),
    ok.


blacklist_table_create() ->
    _Table = ets:new(?FIBS_BLACKLIST_ETS, [set, named_table, public, {keypos,1}]).

blacklist_table_destroy() ->
    true = ets:delete(?FIBS_BLACKLIST_ETS).


%% internal functions
