% The following iterative sequence is defined for the set of positive integers:
%
% n → n/2 (n is even)
% n → 3n + 1 (n is odd)
%
% Using the rule above and starting with 13, we generate the following sequence:
%
% 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
% It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
% Although it has not been proved yet (Collatz Problem), it is thought that all starting 
% numbers finish at 1.
%
% Which starting number, under one million, produces the longest chain?
%
% NOTE: Once the chain starts the terms are allowed to go above one million.

-module(problem0014).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(N) ->
    [{BestStarter,Length}|_Tail] = lists:reverse(lists:keysort(2, sequences(N))),
    io:format("The longest sequence started with ~p, and is ~p numbers long. It is:~n~p~n", [BestStarter, Length, collatz(BestStarter)]),
    BestStarter.

sequences(N) ->
    lists:map(fun(I) -> {I, length(collatz(I))} end, lists:seq(1,N)).

collatz(Start) ->
    lists:reverse(collatz(Start, [])).

collatz(1, List) -> [1 | List];
collatz(N, List) when N rem 2 == 0 -> collatz(N div 2, [N | List]);
collatz(N, List) when N rem 2 == 1 -> collatz(3 * N + 1, [N | List]).

collatz_test_() ->
    [?_assertEqual([13,40,20,10,5,16,8,4,2,1], collatz(13))
    ,?_assertEqual([10,5,16,8,4,2,1], collatz(10))
    ].
sequences_test_() ->
    [?_assertEqual([{1,1},{2,2},{3,8},{4,3}], sequences(4))
    ,?_assertEqual([{1,1},{2,2},{3,8},{4,3},{5,6}], sequences(5))
    ].
solve_test_() ->
    [?_assertEqual(3, solve(4))
    ,?_assertEqual(3, solve(5))
    ].
