% If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
% Find the sum of all the multiples of 3 or 5 below 1000.

lists:foldl( fun(X,Sum) -> if X rem 5 == 0 -> X+Sum; X rem 3 == 0 -> X+Sum; true -> Sum end end, 0, lists:seq(1,999)).
