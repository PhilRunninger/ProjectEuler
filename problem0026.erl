%%% vim:foldmethod=marker
-module(problem0026).

-export([solve/0, all/0]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

solve() ->
    lists:max(
      lists:map(
        fun({Period, _Digits}) ->
                Period
        end, lists:map(
               fun(N) -> 
                       (cycle(N)) 
               end, lists:seq(2,999)
              )
       )
     ).

all() ->
    lists:map(
      fun(N) -> 
              io:format("~p: ~p~n", [N, cycle(N)])
      end, lists:seq(2,999)
     ).

%%% Internal functions   {{{1

cycle(N) ->
    cycle(1, N, [], []).

cycle(0, _Denominator, Digits, Numerators) ->
    {period(0, Numerators), lists:reverse(Digits)};
cycle(Numerator, Denominator, Digits, Numerators) ->
    cycle(Numerator, Denominator, Digits, Numerators, lists:member(Numerator, Numerators)).

cycle(Numerator, _Denominator, Digits, Numerators, true) ->
    {period(Numerator, Numerators), lists:reverse(Digits)};
cycle(Numerator, Denominator, Digits, Numerators, false) ->
    cycle((10 * Numerator) rem Denominator, Denominator, [(10 * Numerator) div Denominator | Digits], [Numerator | Numerators]).

period(Numerator, Numerators) -> period(Numerator, Numerators, 1).

period(_, [], _) -> 0;
period(N, [N | _T], Index) -> Index;
period(N, [_ | T], Index) -> period(N, T, Index+1).

%%% Unit Tests   {{{1
-ifdef(EUNIT).

cycle_test_() ->
    [
     ?_assertEqual({0, [5]}, cycle(2)),
     ?_assertEqual({0, [2,5]}, cycle(4)),
     ?_assertEqual({1, [3]}, cycle(3)),
     ?_assertEqual({1, [1,6]}, cycle(6)),
     ?_assertEqual({6, [1,4,2,8,5,7]}, cycle(7))
    ].

period_test_() ->
    [
     ?_assertEqual(0, period(0, [1])),
     ?_assertEqual(1, period(3, [3])),
     ?_assertEqual(1, period(6, [6,1])),
     ?_assertEqual(6, period(1, [7,5,8,2,4,1]))
    ].

-endif.


% To test, in vim,                   :! (erlc % && erl -s %:r test -s init stop -noinput)
