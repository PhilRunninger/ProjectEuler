% Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
% If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
%
% For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
% The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
%
% Evaluate the sum of all the amicable numbers under 10000.

-module(problem0021).
-export([solve/1]).
solve(Limit) ->
  A = lists:map(fun(N) -> 
                  {N, lists:foldl(fun(Sum,X)->Sum+X end,0,
                        [X || X <- lists:seq(1,N div 2), N rem X == 0])
                  } 
                end, 
                lists:seq(1,Limit)),
  B = lists:filter(fun({N, Sum}) -> lists:member({Sum,N},A) andalso N =/= Sum end, A),
  lists:foldl(fun({X,_},Sum)->X+Sum end, 0, B).
