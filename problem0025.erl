% The Fibonacci sequence is defined by the recurrence relation:
%
% Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
% Hence the first 12 terms will be:
%
% F1 = 1
% F2 = 1
% F3 = 2
% F4 = 3
% F5 = 5
% F6 = 8
% F7 = 13
% F8 = 21
% F9 = 34
% F10 = 55
% F11 = 89
% F12 = 144
% The 12th term, F12, is the first term to contain three digits.
%
% What is the first term in the Fibonacci sequence to contain 1000 digits?

-module(problem0025).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib(Limit) -> fib([1,1], Limit, true).
fib([A,B|Tail], Limit, true) -> fib([A+B, A, B | Tail], 
                                    Limit, 
                                    length(integer_to_list(A + B)) < Limit);
fib(List, _, false) ->
    length(List).

fib_test_() ->
    [?_assertEqual(7, fib(2))
    ,?_assertEqual(12, fib(3))
    ].
