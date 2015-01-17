% The prime factors of 13195 are 5, 7, 13 and 29.
% What is the largest prime factor of the number 600851475143 ?

-module(problem0003).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(N) ->
    lists:max(primes(N, 2, [])).

primes(1, _, Factors) -> 
    lists:usort(Factors);
primes(N, F, Factors) when N rem F == 0 ->
    primes(N div F, F, [F | Factors]);
primes(N, F, Factors) ->
    primes(N, F+1, Factors).

primes_test_() ->
    [?_assertEqual([2], primes(2, 2, []))
    ,?_assertEqual( 2 , solve(2))
    ,?_assertEqual([3], primes(3, 2, []))
    ,?_assertEqual( 3 , solve(3))
    ,?_assertEqual([2,3], primes(6, 2, []))
    ,?_assertEqual(   3 , solve(3))
    ,?_assertEqual([3], primes(9, 2, []))
    ,?_assertEqual( 3 , solve(9))
    ,?_assertEqual([2,3,7], primes(42, 2, []))
    ,?_assertEqual(     7 , solve(42))
    ,?_assertEqual([71,839,1471,6857], primes(600851475143, 2, []))
    ,?_assertEqual(             6857 , solve(600851475143))
    ].
