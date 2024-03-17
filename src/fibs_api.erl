-module(fibs_api).

-export([
    generate_rev/2,

    blacklist_add/1,
    blacklist_remove/1,
    blacklist_has/1
]).

-include("include/fibs.hrl").


-spec generate_rev(
        NumCount :: integer(),
        Prev :: undefined | [integer()]
    ) -> [integer()].
% Generate sequence of fibonacci numbers with specified length.
% If optional list of previous numbers is specified, continue generation of
% sequence from those previous numbers.
%
% Handle special cases for N=1,2, pass to generic recursive function for N>2
% with pre-generated accumulator of first two numbers
generate_rev(1, undefined = _Prev) ->
    [0];

generate_rev(2, undefined = _Prev) ->
    [1, 0];

generate_rev(N, undefined = _Prev) ->
    Acc = [1, 0],
    generate_rev_impl(N-2, Acc);

generate_rev(1, [A, B] = _Prev) ->
    [A + B];

generate_rev(2, [A, B] = _Prev) ->
    [A+A+B, A+B];

generate_rev(N, [A, B] = _Prev) ->
    Acc = [A+A+B, A+B],
    generate_rev_impl(N-2, Acc).


% Generic function to generate fibonacci sequence using accumulator of already generated numbers.
% Requires the accumulator to have at least 2 numbers
generate_rev_impl(0, Acc) ->
    Acc;
generate_rev_impl(N, [A, B | _] = Acc) ->
    generate_rev_impl(N-1, [A+B | Acc]).


% blacklist functions
blacklist_add(X) when is_integer(X) ->
    true = ets:insert(?FIBS_BLACKLIST_ETS, {X}).

blacklist_remove(X) when is_integer(X) ->
    true = ets:delete(?FIBS_BLACKLIST_ETS, X).

blacklist_has(X) when is_integer(X) ->
    case ets:lookup(?FIBS_BLACKLIST_ETS, X) of
        [_] -> true;
        [] -> false
    end.


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


blacklist_table_test_() ->
    {
        setup,
        fun() -> fibs_app:blacklist_table_create() end,
        fun(_) -> fibs_app:blacklist_table_destroy() end,
        [
            fun test_blacklist_table/0
        ]
    }.


test_blacklist_table() ->
    ?assertNot(blacklist_has(123)),
    ?assertNot(blacklist_has(456)),

    blacklist_add(123),
    ?assert(blacklist_has(123)),
    ?assertNot(blacklist_has(456)),

    blacklist_add(456),
    ?assert(blacklist_has(123)),
    ?assert(blacklist_has(456)),

    blacklist_remove(123),
    ?assertNot(blacklist_has(123)),
    ?assert(blacklist_has(456)),

    % repeat, check blacklist is still usable
    blacklist_remove(123),
    ?assertNot(blacklist_has(123)),
    ?assert(blacklist_has(456)),

    blacklist_remove(456),
    ?assertNot(blacklist_has(123)),
    ?assertNot(blacklist_has(456)).


-endif.

