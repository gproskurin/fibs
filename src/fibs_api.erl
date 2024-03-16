-module(fibs_api).


-export([
    generate_rev/2
]).


generate_rev(1, undefined = _Prev) ->
    [0];
generate_rev(2, undefined = _Prev) ->
    [1, 0];
generate_rev(N, undefined = _Prev) when N > 2 ->
    Acc = [1, 0],
    generate_rev_impl(N-2, Acc);

generate_rev(1, [A, B] = _Prev) ->
    [A + B];

generate_rev(2, [A, B] = _Prev) ->
    [A+A+B, A+B];

generate_rev(N, [A, B] = _Prev) ->
    Acc = [A+A+B, A+B],
    generate_rev_impl(N-2, Acc).


generate_rev_impl(0, Acc) ->
    Acc;
generate_rev_impl(N, [A, B | _] = Acc) ->
    generate_rev_impl(N-1, [A+B | Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


fib_test() ->
    ?assertEqual([0], generate_rev(1, undefined)),
    ?assertEqual([1,0], generate_rev(2, undefined)),
    ?assertEqual([1,1,0], generate_rev(3, undefined)),
    ?assertEqual([2,1,1,0], generate_rev(4, undefined)),
    ?assertEqual([3,2,1,1,0], generate_rev(5, undefined)),
    ?assertEqual([5,3,2,1,1,0], generate_rev(6, undefined)),
    ?assertEqual([8,5,3,2,1,1,0], generate_rev(7, undefined)),
    Fbig1 = generate_rev(10000, undefined), % make sure complexity is not exponential
    ?assertEqual(10000, length(Fbig1)),

    ?assertEqual([8], generate_rev(1, [5,3])),
    ?assertEqual([13,8], generate_rev(2, [5,3])),
    ?assertEqual([21,13,8], generate_rev(3, [5,3])),
    ?assertEqual([34,21,13,8], generate_rev(4, [5,3])),
    Fbig2 = generate_rev(20000, [10,20]),
    ?assertEqual(20000, length(Fbig2)),

    ?assertEqual([41,25,16,9], generate_rev(4, [7,2])).


-endif.

