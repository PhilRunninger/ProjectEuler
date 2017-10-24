%%% vim:foldmethod=marker
-module(problem0044).

-export([go/0, go/1]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

go() -> go(10).

go(N) ->
    X = [{A, B} || A <- lists:map(fun(X) -> pentagonal(X) end, lists:seq(1,N)),
                   B <- lists:map(fun(X) -> pentagonal(X) end, lists:seq(1,N)),
                   A<B,
                   is_pentagonal(A+B),
                   is_pentagonal(B-A)
        ].

%%% Internal functions   {{{1
pentagonal(N) -> trunc(N*(3*N-1)/2).

is_pentagonal(N) ->
    Root = (0.5 + math:sqrt(0.25 + 6 * N))/3,
    Root == round(Root).

%%% Unit Tests   {{{1
pentagonal_test_() ->
    [
     ?_assertEqual(1, pentagonal(1)),
     ?_assertEqual(5, pentagonal(2)),
     ?_assertEqual(117, pentagonal(9))
    ].

is_pentagonal_test_() ->
    [
     ?_assert(is_pentagonal(1)),
     ?_assertNot(is_pentagonal(2)),
     ?_assert(is_pentagonal(22))
    ].
