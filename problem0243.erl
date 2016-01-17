% A positive fraction whose numerator is less than its denominator is called a proper fraction.
% For any denominator, d, there will be d−1 proper fractions; for example, with d = 12:
% 1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

% We shall call a fraction that cannot be cancelled down a resilient fraction.
% Furthermore we shall define the resilience of a denominator, R(d), to be the ratio of its proper fractions that are resilient; for example, R(12) = 4/11 .
% In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .

% Find the smallest denominator d, having a resilience R(d) < 15499/94744.
%
% 24 = 2*2*2*3     1         2
%         12345678901234567890123
%      2:  x x x x x x x x x x x   23 div 2 = 11
%      3:   x  x  x  x  x  x  x    23 div 3 =  7
%    2*3:      x     x     x       23 div 6 =  3    11+7-3 = 15
%  Final:  xxx x xxx x xxx x xxx
%
% 30 = 2*3*5       1         2
%         12345678901234567890123456789
%      2:  x x x x x x x x x x x x x x     29 div 2 = 14
%      3:   x  x  x  x  x  x  x  x  x      29 div 3 =  9
%      5:     x    x    x    x    x        29 div 5 =  5
%    2*3:      x     x     x     x         29 div 6 =  4
%    2*5:          x         x             29 div 10 = 2
%    3*5:               x                  29 div 15 = 1   14+9+5-4-2-1 = 21
%  Final:  xxxxx xxx x xxx x xxx xxxxx
%

-module(problem0243).
-export([solve/1,solve/2,resilience/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Threshold) -> solve(2, Threshold).
solve(Start, Threshold) -> solve(Start, Threshold, {1.0,1}).
solve(Denominator, Threshold, MinResilience) -> solve(Denominator,Threshold,resilience(Denominator),MinResilience).
solve(Denominator, Threshold, {Res,_} = Resilience, MinResilience) when Res<Threshold ->
  io:format("Resilience:~p  Min:~p~n", [Resilience, MinResilience]),
  Denominator;
solve(Denominator, Threshold, {Res,_}=Resilience, {Min,_}=MinResilience) when Res<Min ->
  io:format("Resilience:~p  Min:~p~n", [Resilience, MinResilience]),
  solve(Denominator+1, Threshold, lists:min([Resilience, MinResilience]));
solve(Denominator, Threshold, Resilience, MinResilience) ->
  solve(Denominator+1, Threshold, lists:min([Resilience, MinResilience])).

resilience(Denominator) ->
  Factors = factors:prime_factors(Denominator),
  Combinations = combinatorics:combinations(length(Factors), Factors),
  UniqueCombos = lists:usort(lists:map(fun(A) -> lists:usort(A) end, Combinations)),
  ReducibleCount = lists:foldl(fun(Xs,Sum)->
                                   Sum + ((Denominator-1) div lists:foldl(fun(X,Product)->
                                                                              X*Product
                                                                          end, 1, Xs)) * math:pow(-1,length(Xs)-1)
                               end, 0, UniqueCombos),
  {1 - ReducibleCount / (Denominator-1), Denominator}.

resilience_test_() ->
  [
    ?_assertEqual({4/11,12}, resilience(12))
   ,?_assertEqual({4/7,8}, resilience(8))
   ,?_assertEqual({6/6,7}, resilience(7))
  ].
solve_test_() ->
  [
    ?_assertEqual(12, solve(4/10))
   ,?_assertEqual(4, solve(0.67)) ].
