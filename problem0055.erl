%%% vim:foldmethod=marker
% If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
% Not all numbers produce palindromes so quickly. For example,
%       349 + 943 = 1292,
%       1292 + 2921 = 4213
%       4213 + 3124 = 7337
% That is, 349 took three iterations to arrive at a palindrome.
% Although no one has proved it yet, it is thought that some numbers,
% like 196, never produce a palindrome. A number that never forms a
% palindrome through the reverse and add process is called a Lychrel
% number. Due to the theoretical nature of these numbers, and for the
% purpose of this problem, we shall assume that a number is Lychrel
% until proven otherwise. In addition you are given that for every
% number below ten-thousand, it will either (i) become a palindrome in
% less than fifty iterations, or, (ii) no one, with all the computing
% power that exists, has managed so far to map it to a palindrome. In
% fact, 10677 is the first number to be shown to require over fifty
% iterations before producing a palindrome:
% 4668731596684224866951378664 (53 iterations, 28-digits).  Surprisingly,
% there are palindromic numbers that are themselves Lychrel numbers;
% the first example is 4994.
%
% How many Lychrel numbers are there below ten-thousand?

-module(problem0055).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

solve() ->
    solve(50).

solve(MaxIter) ->
    length([X || X <- lists:seq(1,10000,1), is_lychrel(X, MaxIter)]).

%%% Internal functions   {{{1
flip(N) -> list_to_integer(lists:reverse(integer_to_list(N))).

is_palindrome(N) -> N == flip(N).

is_lychrel(N, MaxIter) ->
    Test = N + flip(N),
    is_lychrel(Test, is_palindrome(Test), 1, MaxIter).

is_lychrel(_, true, _, _) -> false;
is_lychrel(_, false, MaxIter, MaxIter) -> true;
is_lychrel(N, false, Iterations, MaxIter) ->
    Test = N + flip(N),
    is_lychrel(Test, is_palindrome(Test), Iterations+1, MaxIter).

%%% Unit Tests   {{{1
flip_number_test_() ->
    [
      ?_assertEqual(1, flip(1))
     ,?_assertEqual(1, flip(10))
     ,?_assertEqual(11, flip(11))
     ,?_assertEqual(12, flip(21))
    ].

is_palindrome_test_() ->
    [
      ?_assertEqual(true, is_palindrome(1))
     ,?_assertEqual(false, is_palindrome(12))
     ,?_assertEqual(true, is_palindrome(121))
    ].

is_lychrel_test_() ->
    [
      ?_assertEqual(false, is_lychrel(1, 50))
     ,?_assertEqual(true, is_lychrel(4994, 50))
     ,?_assertEqual(true, is_lychrel(349, 2))
     ,?_assertEqual(false, is_lychrel(349, 3))
    ].
