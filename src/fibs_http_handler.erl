-module(fibs_http_handler).

-behaviour(cowboy_rest).

% cowboy REST callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2
]).

-export([
    handle_from_json/2
]).

-include_lib("kernel/include/logger.hrl").


init(Req, State) ->
    {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, []}, handle_from_json}
        ],
        Req,
        State
    }.


handle_from_json(Req, State) ->
    case parse_params(Req) of
        {ok, Params} ->
            Result = maybe_encode_continuation(fib_process(Params)),
            {fibs_util:to_json(Result), Req, State};
        {error, {validation_error, Msg}} ->
            {fibs_util:to_json(#{<<"error">> => Msg}), Req, State} % TODO return error code
    end.


maybe_encode_continuation(#{<<"continuation">> := C} = M) ->
    M#{<<"continuation">> => fibs_util:continuation_encode(C)};
maybe_encode_continuation(M) ->
    M.


fib_process(Params) ->
    PageSize = maps:get(pagesize, Params),

    {Count, Prev} = case Params of
        #{count := C} ->
            {C, []};
        #{continuation := #{<<"rem_count">> := C, <<"prev">> := P}} ->
            {C, P}
    end,

    case Count > PageSize of
        true ->
            % numbers don't fit one page
            {NewPrev, FibRev} = fibs_api:generate_rev(PageSize, Prev),
            NewContinuation = #{
                <<"prev">> => NewPrev,
                <<"rem_count">> => Count - PageSize
            },
            #{
                <<"numbers">> => lists:reverse(FibRev),
                <<"continuation">> => NewContinuation
            };
        false ->
            % all remaining numbers will fit in one page, no new continuation
            {_NewPrev, FibRev} = fibs_api:generate_rev(Count, Prev),
            #{
                <<"numbers">> => lists:reverse(FibRev)
            }
    end.


parse_params(Req) ->
    Qs = cowboy_req:parse_qs(Req),

    Continuation = case proplists:get_value(<<"continuation">>, Qs, undefined) of
        undefined ->
            undefined;
        C ->
            % TODO validate, check errors
            fibs_util:continuation_decode(C)
    end,

    Count0 = proplists:get_value(<<"count">>, Qs, undefined),
    Count = case Count0 of
        undefined -> undefined;
        _ -> binary_to_integer(Count0)
    end,

    Param0 = case {Count, Continuation} of
        {_, undefined} when Count > 0 ->
            #{count => Count};
        {undefined, _} when is_map(Continuation) ->
            #{continuation => Continuation}
    end,

    PageSize = binary_to_integer(proplists:get_value(<<"pagesize">>, Qs, <<"100">>)),
    case PageSize < 2 of
        true ->
            % TODO support pagesize=1
            {error, {validation_error, <<"pagesize_must_be_at_least_2">>}};
        false ->
            Param = Param0#{pagesize => PageSize},
            ?LOG_NOTICE("Parsed parameters: ~p", [Param]),
            {ok, Param}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


handler_test_() ->
    {
        setup,
        fun() -> fibs_app:blacklist_table_create() end,
        fun(_) -> fibs_app:blacklist_table_destroy() end,
        [
            fun test_fib_process_onepage/0,
            fun test_fib_process_pagination1/0,
            fun test_fib_process_pagination2/0,
            fun test_fib_process_blacklist/0
        ]
    }.


test_fib_process_onepage() ->
    ?assertEqual(#{<<"numbers">> => [0]}, fib_process(#{count => 1, pagesize => 10})),
    ?assertEqual(#{<<"numbers">> => [0,1]}, fib_process(#{count => 2, pagesize => 10})),
    ?assertEqual(#{<<"numbers">> => [0,1,1,2,3,5,8]}, fib_process(#{count => 7, pagesize => 10})),
    ?assertEqual(#{<<"numbers">> => [0,1,1,2,3,5,8]}, fib_process(#{count => 7, pagesize => 7})).


test_fib_process_pagination1() ->
    #{<<"numbers">> := [0,1,1,2], <<"continuation">> := C1} = fib_process(#{count => 5, pagesize => 4}),
    ?assertEqual(#{<<"rem_count">> => 1, <<"prev">> => [2,1]}, C1),
    ?assertEqual(#{<<"numbers">> => [3]}, fib_process(#{continuation => C1, pagesize => 4})).


test_fib_process_pagination2() ->
    R1 = fib_process(#{count => 10, pagesize => 4}),
    C1 = maps:get(<<"continuation">>, R1),
    ?assertEqual(#{<<"numbers">> => [0,1,1,2], <<"continuation">> => C1}, R1),
    ?assertEqual(#{<<"rem_count">> => 6, <<"prev">> => [2,1]}, C1),

    R2 = fib_process(#{continuation => C1, pagesize => 4}),
    C2 = maps:get(<<"continuation">>, R2),
    ?assertEqual(#{<<"numbers">> => [3,5,8,13], <<"continuation">> => C2}, R2),
    ?assertEqual(#{<<"rem_count">> => 2, <<"prev">> => [13,8]}, C2),

    R3 = fib_process(#{continuation => C2, pagesize => 4}),
    ?assertEqual(#{<<"numbers">> => [21,34]}, R3).


test_fib_process_blacklist() ->
    fibs_api:blacklist_add(0),
    fibs_api:blacklist_add(1),
    fibs_api:blacklist_add(8),
    R1 = fib_process(#{count => 7, pagesize => 4}),
    C1 = maps:get(<<"continuation">>, R1),
    ?assertEqual(#{<<"numbers">> => [2,3,5,13], <<"continuation">> => C1}, R1),
    ?assertEqual(#{<<"rem_count">> => 3, <<"prev">> => [13,8]}, C1),

    fibs_api:blacklist_add(34),
    R2 = fib_process(#{continuation => C1, pagesize => 4}),
    ?assertEqual(#{<<"numbers">> => [21,55,89]}, R2).


http_req(Params) ->
    Url = <<"http://localhost:8000/fibs/generate?", Params/binary>>,
    %?debugFmt("URL=~p", [Url]),
    {ok, {{_, 200, _}, _Heaers, Body}} = httpc:request(get, {Url,[]}, [], [{sync,true}]),
    %?debugFmt("Body=~p", [Body]),
    Rjson = fibs_util:from_json(Body),
    %?debugFmt("Rjson=~p", [Rjson]),
    Rjson.

fib_http_test_() ->
    {
        setup,
        fun() ->
            {ok, _} = application:ensure_all_started(fibs)
        end,
        fun(_) ->
            ok = application:stop(fibs)
        end,
        [
            fun test_http_basic/0,
            fun test_http_basic2/0,
            fun test_http_cont/0
        ]
    }.


test_http_basic() ->
    R = http_req(<<"count=8">>),
    ?assertEqual(#{<<"numbers">> => [0,1,1,2,3,5,8,13]}, R).


test_http_basic2() ->
    R = http_req(<<"count=150">>),
    ?assertMatch(#{<<"numbers">> := [_|_]}, R).


test_http_cont() ->
    R1 = http_req(<<"count=10&pagesize=5">>),
    C1 = maps:get(<<"continuation">>, R1),
    ?assertEqual(#{<<"numbers">> => [0,1,1,2,3], <<"continuation">> => C1}, R1),

    R2 = http_req(<<"continuation=",C1/binary,"&pagesize=2">>), % change pagesize
    C2 = maps:get(<<"continuation">>, R2),
    ?assertEqual(#{<<"numbers">> => [5,8], <<"continuation">> => C2}, R2),

    R3 = http_req(<<"continuation=",C2/binary>>),
    ?assertEqual(#{<<"numbers">> => [13,21,34]}, R3).


-endif.

