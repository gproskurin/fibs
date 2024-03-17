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
        Prev :: [integer()]
    ) -> {AllFibNumbers :: [integer()], FilteredFibNumbers :: [integer()]}.
% Generate sequence of fibonacci numbers with specified length.
% Use list of previous numbers to continue generation of sequence.
generate_rev(N, Prev) ->
    generate_rev_impl(N, Prev, []).


generate_rev_impl(0, All, Filtered) ->
    {All, Filtered};
generate_rev_impl(N, All, Filtered) ->
    {FibNum, NewAll} = case All of
        [A, B | _] ->
            F = A + B,
            {F, [F, A]}; % we need only last two numbers from "All"
        [] ->
            {0, [0]};
        [0] ->
            {1, [1, 0]}
    end,
    case blacklist_has(FibNum) of
        true ->
            generate_rev_impl(N, NewAll, Filtered);
        false ->
            generate_rev_impl(N-1, NewAll, [FibNum | Filtered])
    end.


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


blacklist_table_test_() ->
    {
        setup,
        fun() -> fibs_app:blacklist_table_create() end,
        fun(_) -> fibs_app:blacklist_table_destroy() end,
        [
            fun test_fib/0,
            fun test_blacklist_table/0
        ]
    }.


test_fib() ->
    ?assertEqual({[0], [0]}, generate_rev(1, [])),
    ?assertEqual({[1,0], [1,0]}, generate_rev(2, [])),
    ?assertEqual({[1,1], [1,1,0]}, generate_rev(3, [])),
    ?assertEqual({[2,1], [2,1,1,0]}, generate_rev(4, [])),
    ?assertEqual({[3,2], [3,2,1,1,0]}, generate_rev(5, [])),
    ?assertEqual({[5,3], [5,3,2,1,1,0]}, generate_rev(6, [])),
    ?assertEqual({[8,5], [8,5,3,2,1,1,0]}, generate_rev(7, [])),
    {[_,_], Fbig1} = generate_rev(10000, []), % make sure complexity is not exponential
    ?assertEqual(10000, length(Fbig1)),

    ?assertMatch({[8,5], [8]}, generate_rev(1, [5,3])),
    ?assertMatch({[13,8], [13,8]}, generate_rev(2, [5,3])),
    ?assertMatch({[21,13], [21,13,8]}, generate_rev(3, [5,3])),
    ?assertMatch({[34,21], [34,21,13,8]}, generate_rev(4, [5,3])),
    {[_,_], Fbig2} = generate_rev(20000, [10,20]),
    ?assertEqual(20000, length(Fbig2)),

    ?assertEqual({[41,25], [41,25,16,9]}, generate_rev(4, [7,2])).


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

