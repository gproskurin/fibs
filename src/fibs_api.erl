-module(fibs_api).


-export([
    generate_rev/2
]).


generate_rev(0, Acc) ->
    Acc;

generate_rev(N, [A, B | _] = Acc) ->
    generate_rev(N-1, [A + B | Acc]);

generate_rev(N, []) ->
    generate_rev(N-1, [0]);

generate_rev(N, [_] = Acc) ->
    generate_rev(N-1, [1 | Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


fib_test() ->
    ?assertEqual([], generate_rev(0, [])),
    ?assertEqual([42], generate_rev(0, [42])),
    ?assertEqual([1,0], generate_rev(2, [])),
    ?assertEqual([1,1,0], generate_rev(3, [])),
    ?assertEqual([2,1,1,0], generate_rev(4, [])),
    ?assertEqual([3,2,1,1,0], generate_rev(5, [])),
    ?assertEqual([5,3,2,1,1,0], generate_rev(6, [])),
    ?assertEqual([8,5,3,2,1,1,0], generate_rev(7, [])),
    ?assertMatch([_|_], generate_rev(10000, [])). % make sure complexity is not exponential


-endif.

