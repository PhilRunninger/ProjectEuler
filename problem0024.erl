-module(problem0024).

-compile([export_all]).

solve() ->
    lists:nth(1000000,combinatorics:permutations([0,1,2,3,4,5,6,7,8,9])).
