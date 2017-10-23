%%% vim:foldmethod=marker

%  The first two consecutive numbers to have two distinct prime factors are:
%      14 = 2 * 7
%      15 = 3 * 5
%
%  The first three consecutive numbers to have three distinct prime factors are:
%      644 = 2 * 2 * 7 * 23
%      645 = 3 * 5 * 43
%      646 = 2 * 17 * 19.
%
%  Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
-module(problem0047).

-export([go/1]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

go(N) ->
    check(N, init_list(N, N), 0, init_list(N, 0)).

%%% Internal functions   {{{1
init_list(Size, Value) ->
    [Value || _ <- lists:seq(1,Size)].

check(N, StopCondition, X, StopCondition) ->
    X - N + 1;
check(N, StopCondition, X, [_|T]) ->
    check(N, StopCondition, X+1, T ++ [length(factors:prime_factors(X+1))]).

%%% Unit Tests   {{{1
init_list_test_() ->
    [
     ?_assertEqual([1], init_list(1,1)),
     ?_assertEqual([0,0], init_list(2,0)),
     ?_assertEqual([3,3,3], init_list(3,3))
    ].
