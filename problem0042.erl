%%% vim:foldmethod=marker

% The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
% so the first ten triangle numbers are:
%       1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
% By converting each letter in a word to a number corresponding to its alphabetical
% position and adding these values we form a word value. For example, the word
% value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number
% then we shall call the word a triangle word.
%
% Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
% containing nearly two-thousand common English words, how many are triangle words?

-module(problem0042).

-export([solve/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API   {{{1

solve() ->
    {ok, Text} = file:read_file("p042_words.txt"),
    Words = lists:map(fun(W) -> [NoQuotes] = string:tokens(W, "\""), NoQuotes end, string:tokens(binary_to_list(Text), ",")),
    Numbers = lists:map(fun(Word) -> lists:sum(lists:map(fun(Char) -> Char - $A + 1 end, Word)) end, Words),
    IsTriangle = lists:map(fun(N) -> Check = math:sqrt(8*N+1), Check == round(Check) end, Numbers),
    length([X || X<-IsTriangle, X==true]).

%%% Internal functions   {{{1
%%% Unit Tests   {{{1
-ifdef(EUNIT).
-endif.
