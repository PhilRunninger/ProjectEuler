% A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
%
% For example,
%
% 44 → 32 → 13 → 10 → 1 → 1
% 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
%
% Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
%
% How many starting numbers below ten million will arrive at 89?

-module(problem0092).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Limit)        -> solve(Limit, 0).
solve(0, Count)     -> Count;
solve(N, Count)     -> solve(N, Count, lists:last(sequence(N))).
solve(N, Count, 89) -> solve(N-1, Count+1);
solve(N, Count, _)  -> solve(N-1, Count).

sequence(N)                          -> lists:reverse(sequence(N, [], false)).
sequence(_N, [1|_T]=Sequence, true)  -> Sequence;
sequence(_N, [89|_T]=Sequence, true) -> Sequence;
sequence(N, Sequence, _Duplicate)    -> sequence(ssq_of_digits(N), [N | Sequence], lists:member(N, Sequence)).

ssq_of_digits(N)      -> ssq_of_digits(N, 0).
ssq_of_digits(0, Ssq) -> Ssq;
ssq_of_digits(N, Ssq) -> ssq_of_digits(N div 10, Ssq + round(math:pow(N rem 10,2))).

ssq_of_digits_test_() ->
    [?_assertEqual(53, ssq_of_digits(72))
    ,?_assertEqual(17, ssq_of_digits(14))
    ].
sequence_test_() ->
    [?_assertEqual([44,32,13,10,1,1], sequence(44))
    ,?_assertEqual([85,89,145,42,20,4,16,37,58,89], sequence(85))
    ,?_assertEqual([4,16,37,58,89,145,42,20,4,16,37,58,89], sequence(4))
    ].
solve_test_() ->
    [?_assertEqual(3, solve(4))  % 4,16,37,58,89,...,89  3,9,81,65,61,37,58,89,...89  2,4,...,89  1,1
    ,?_assertEqual(4, solve(5))  % 5,25,29,85,89,...,89
    ].
