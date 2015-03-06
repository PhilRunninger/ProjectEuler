% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
% What is the 10,001st prime number?

-module(problem0007).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Count) ->
    [Largest|_Rest] = primes:first(Count),
    Largest.

primes_test_() ->
    [
     ?_assertEqual( 2,  solve(1))
    ,?_assertEqual(     5,  solve(3))
    ,?_assertEqual(            13,  solve(6))
    ].
