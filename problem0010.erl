% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
% Find the sum of all the primes below two million.

-module(problem0010).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Max) ->
    lists:foldl(fun(Prime,Acc) -> Acc+Prime end, 0, primes(Max)).

% http://stackoverflow.com/a/599002/510067
primes(Prime, Max, Primes,Integers) when Prime > Max ->
    lists:reverse([Prime|Primes]) ++ Integers;
primes(Prime, Max, Primes, Integers) ->
    [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
    primes(NewPrime, Max, [Prime|Primes], NewIntegers).

primes(N) ->
    primes(2, round(math:sqrt(N)), [], lists:seq(3,N,2)). % skip odds

primes_test_() ->
    [?_assertEqual([2,3,5,7], primes(10))
    ,?_assertEqual([2,3,5,7,11,13,17,19], primes(20))
    ].
solve_test_() ->
    [?_assertEqual(17, solve(10))
    ].
