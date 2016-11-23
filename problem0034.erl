% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
% Find the sum of all numbers which are equal to the sum of the factorial of their digits.
% Note: as 1! = 1 and 2! = 2 are not sums they are not included.

lists:sum([X || X<-lists:seq(10,2177280,1), X == lists:sum(lists:map(fun(D) -> factorial:compute(D-48) end, integer_to_list(X))) ]).
