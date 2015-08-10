% Euler discovered the remarkable quadratic formula:
%
% n² + n + 41
%
% It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
% However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly
% when n = 41, 41² + 41 + 41 is clearly divisible by 41.
%
% The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive
% values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
%
% Considering quadratics of the form:
%
% n² + an + b, where |a| < 1000 and |b| < 1000
%
% where |n| is the modulus/absolute value of n
% e.g. |11| = 11 and |−4| = 4
% Find the product of the coefficients, a and b, for the quadratic expression that produces the
% maximum number of primes for consecutive values of n, starting with n = 0.

-module(problem0027).
-export([solve/0]).
-include_lib("eunit/include/eunit.hrl").

%--------------------------------------------------------------------------------
candidate(A, B, N) ->
  N*N + A*N + B.

primes_series(A, B) ->
  primes_series(A, B, [], 0, primes:is_prime(abs(candidate(A, B, 0)))).
primes_series(_A, _B, Primes, _N, false) ->
  Primes;
primes_series(A, B, Primes, N, true) ->
  primes_series(A, B, Primes ++ [candidate(A, B, N)], N+1, primes:is_prime(abs(candidate(A, B, N+1)))).

solve() ->
  BCandidates = lists:sort(primes:less_than(1000) ++ lists:map(fun(X) -> -X end, primes:less_than(1000))),
  Results = [{A, B, length(primes_series(A,B))} || A <- lists:seq(-1000, 1000), B <- BCandidates],
  {A,B,Longest} = hd(lists:reverse(lists:keysort(3,Results))),
  io:format("N*N + ~p*N + ~p = ~p", [A,B, primes_series(A,B)]),
  io:format("~p primes", [Longest]),
  A*B.

%--------------------------------------------------------------------------------
primes_series_test_() ->
  [
    ?_assertEqual([2], primes_series(1,2))
   ,?_assertEqual([5,7,11,17], primes_series(1,5))
  ].
