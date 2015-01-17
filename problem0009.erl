% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
%
% a^2 + b^2 = c^2
% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
%
% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.

-module(problem0009).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

solve() -> 0.

is_triplet([Leg1, Leg2, Hypotenuse]) ->
    math:pow(Leg1,2) + math:pow(Leg2,2) == math:pow(Hypotenuse,2).

problem0009_test_() ->
    [
        ?_assertEqual(true, is_triplet([3,4,5]))
       ,?_assertEqual(false, is_triplet([3,4,15]))
    ].
