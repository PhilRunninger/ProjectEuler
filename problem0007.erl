% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
% What is the 10,001st prime number?

-module(problem0007).
-export([primes/1, solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Count) ->
    [Largest|_Rest] = primes(Count),
    Largest.

primes(Count) ->
    primes(Count-1, 3, [2]).

primes(0, _, Primes) -> 
    Primes;
primes(Count, Candidate, Primes) ->
    Limit = lists:max([2 , math:sqrt(Candidate)]),
    case lists:min(lists:filtermap(fun(X) -> if X =< Limit -> {true, Candidate rem X}; true -> false end end, Primes)) of
        0 -> primes(Count, Candidate+2, Primes);
        _Else -> primes(Count-1, Candidate+2, [Candidate | Primes])
    end.

primes_test_() ->
    [?_assertEqual([2], primes(1))
    ,?_assertEqual( 2,  solve(1))
    ,?_assertEqual([5,3,2], primes(3))
    ,?_assertEqual(     5,  solve(3))
    ,?_assertEqual([13,11,7,5,3,2], primes(6))
    ,?_assertEqual(            13,  solve(6))
    ,?_assertEqual(6857,  solve(10001))
    ].
