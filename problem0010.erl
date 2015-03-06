% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
% Find the sum of all the primes below two million.

-module(problem0010).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Max) ->
    lists:foldl(fun(Prime,Acc) -> Acc+Prime end, 0, primes:less_than(Max)).

solve_test_() ->
    [?_assertEqual(17, solve(10))
    ].
