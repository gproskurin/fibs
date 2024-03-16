-module(fibs_util).

-export([
    to_json/1,
    continuation_encode/1,
    continuation_decode/1
]).


to_json(Data) ->
    jiffy:encode(Data).


continuation_encode(M) ->
    base64:encode(to_json(M), #{mode => urlsafe}).


continuation_decode(C) ->
    from_json(base64:decode(C, #{mode => urlsafe})).


% internal functions

from_json(J) ->
    jiffy:decode(J, [return_maps]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


json_test() ->
    ?assertEqual(<<"{\"a\":\"b\"}">>, to_json(#{<<"a">> => <<"b">>})),

    D = #{<<"a">> => <<"b">>, <<"n">> => [1,2,3]},
    ?assertEqual(D, from_json(to_json(D))).


continuation_test() ->
    C = #{<<"num">> => [1,2,3], <<"data">> => <<"some Data">>},
    ?assertEqual(C, continuation_decode(continuation_encode(C))).


-endif.

