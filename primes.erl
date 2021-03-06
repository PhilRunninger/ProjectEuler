-module(primes).
-export([less_than/1, first/1, is_prime/1, next/0, next/1]).
-include_lib("eunit/include/eunit.hrl").

%--------------------------------------------------------------------------------
less_than(N) when N =< 2 -> [];
less_than(3) -> [2];
less_than(N) ->
  less_than(2, round(math:sqrt(N)), [], lists:seq(3,N,2)).

% http://stackoverflow.com/a/599002/510067
less_than(Prime, Max, Primes,Integers) when Prime > Max ->
  lists:reverse([Prime|Primes]) ++ Integers;
less_than(Prime, Max, Primes, Integers) ->
  [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
  less_than(NewPrime, Max, [Prime|Primes], NewIntegers).

%--------------------------------------------------------------------------------
first(Count) ->
  first(Count-1, 3, [2]).

first(0, _, Primes) -> Primes;
first(Count, Candidate, Primes) ->
  Limit = lists:max([2 , math:sqrt(Candidate)]),
  case lists:min(lists:filtermap(fun(X) -> if X =< Limit -> {true, Candidate rem X}; 
                                              true -> false 
                                           end 
                                 end, Primes)) of
    0 -> first(Count, Candidate+2, Primes);
    _Else -> first(Count-1, Candidate+2, [Candidate | Primes])
  end.

%--------------------------------------------------------------------------------
is_prime(2) -> true;
is_prime(N) when N < 2 -> false;
is_prime(N) ->
    is_prime(N, 2, math:sqrt(N), N rem 2).

is_prime(_N, _Factor, _Limit, 0) -> false;
is_prime(_N, Factor, Limit, _) when Factor > Limit -> true;
is_prime(N, 2, Limit, _) -> is_prime(N, 3, Limit, N rem 3);
is_prime(N, Factor, Limit, _) -> is_prime(N, Factor+2, Limit, N rem (Factor+2)).

%--------------------------------------------------------------------------------
twin_primes(Primes) ->
  twin_primes(Primes, []).
twin_primes([], TwinPrimes) ->
  TwinPrimes;
twin_primes([_A], TwinPrimes) ->
  TwinPrimes;
twin_primes([A, B | T], TwinPrimes) when B == A+2 ->
  twin_primes([B | T], TwinPrimes ++ [{A,B}]);
twin_primes([_A, B | T], TwinPrimes) ->
  twin_primes([B | T], TwinPrimes).

%--------------------------------------------------------------------------------
next() -> next([]).

next([]) -> {2, [2]};
next([2]) -> next(3, [2]);
next(InPrimes) ->
  [LastPrime|_] = lists:reverse(InPrimes),
  next(LastPrime+2, InPrimes).

next(Candidate, InPrimes) ->
  next(Candidate, not lists:any(fun(X) -> Candidate rem X == 0 end, InPrimes), InPrimes).

next(Candidate, true, InPrimes) ->
  {Candidate, lists:reverse([Candidate | lists:reverse(InPrimes)])};
next(Candidate, false, InPrimes) ->
  next(Candidate+1, InPrimes).

%--------------------------------------------------------------------------------
less_than_test_() ->
  [
    ?_assertEqual([2,3,5,7], less_than(10))
   ,?_assertEqual([2,3,5,7,11,13,17,19], less_than(20))
   ,?_assertEqual([2], less_than(3))
   ,?_assertEqual([], less_than(2))
  ].
first_test_() ->
  [
    ?_assertEqual([2], first(1))
   ,?_assertEqual([5,3,2], first(3))
   ,?_assertEqual([13,11,7,5,3,2], first(6))
  ].
is_prime_test_() ->
  [
    ?_assertEqual(true, is_prime(5))
   ,?_assertEqual(false, is_prime(4))
   ,?_assertEqual(true, is_prime(2))
   ,?_assertEqual(true, is_prime(3))
   ,?_assertEqual(false, is_prime(1))
  ].
twin_primes_test_() ->
  [
    ?_assertEqual([], twin_primes([2,3]))
   ,?_assertEqual([{3,5},{5,7},{11,13}], twin_primes([2,3,5,7,11,13,17]))
  ].
next_test_() ->
  [
    ?_assertEqual({2, [2]}, next())
   ,?_assertEqual({2, [2]}, next([]))
   ,?_assertEqual({3, [2,3]}, next([2]))
   ,?_assertEqual({5, [2,3,5]}, next([2,3]))
  ].
