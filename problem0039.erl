% If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
%      {20,48,52}, {24,45,51}, {30,40,50}
% For which value of p ≤ 1000, is the number of solutions maximised?

-module(problem0039).
-export([solve/1]).

% See problem0009.erl for and explanation of the algorithm simplification.

solve(N) ->
    [H|_] = lists:reverse(lists:sort(lists:map(fun(X) -> Triplets = find_triplets(X), {length(Triplets), X, Triplets} end, lists:seq(1,N,1)))),
    H.

find_triplets(S) ->
  [{A, round(B), round(math:sqrt(A*A+B*B))} ||
       A <- lists:seq(1,S div 2),
       B <- [(S*S - 2*A*S)/(2*(S-A))],
       A<B,
       B == round(B)].

