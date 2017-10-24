%%% vim:foldmethod=marker
-module(problem0050).

-export([go/1]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

go(Max) ->
    Primes = primes:less_than(Max),
    Solutions = lists:sort(fun({Xs,_}, {Ys,_}) -> length(Xs) > length(Ys) end, go(Max, Primes, 1, length(Primes), [])),
    hd(Solutions).

go(_,_,_,1,Solutions) ->
    Solutions;
go(Max, Primes, Max, Len, Solutions) ->
    go(Max, Primes, 1, Len-1, Solutions);
go(Max, Primes, Start, Len, Solutions) ->
    go(Max, Primes, Start, Len, lists:sum(lists:sublist(Primes, Start, Len)), Solutions).

go(Max, Primes, _, Len, SumOfPrimes, Solutions) when SumOfPrimes > Max ->
    go(Max, Primes, 1, Len-1, Solutions);
go(Max, Primes, Start, Len, SumOfPrimes, Solutions) ->
    go(Max, Primes, Start, Len, SumOfPrimes, lists:member(SumOfPrimes, Primes), Solutions).

go(Max, Primes, Start, Len, SumOfPrimes, true, Solutions) ->
    go(Max, Primes, Start+1, Len, [{lists:sublist(Primes, Start, Len), SumOfPrimes} | Solutions]);
go(Max, Primes, Start, Len, _, false, Solutions) ->
    go(Max, Primes, Start+1, Len, Solutions).

%%% Internal functions   {{{1
%%% Unit Tests   {{{1

