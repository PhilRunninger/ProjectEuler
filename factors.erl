-module(factors).

-export([factors/1,common_factors/1,prime_factors/1]).

-include_lib("eunit/include/eunit.hrl").

common_factors({_Numerator,_Denominator}=Tuple) ->
  common_factors(tuple_to_list(Tuple));
common_factors(Prodcts) ->
  Sets = lists:map(fun(Product) -> sets:from_list(factors(Product)) end, Prodcts),
  lists:sort(sets:to_list(sets:intersection(Sets))).

factors(N) -> factors(N, 1, N, []).
factors(_N, F1, F2, Factors) when F1 > F2     -> lists:usort(Factors);
factors(N, F1, F2, Factors) when F1 * F2 == N -> factors(N, F1+1, round(N / (F1+1)), Factors ++ [F1, F2]);
factors(N, F1, _F2, Factors)                  -> factors(N, F1+1, round(N / (F1+1)), Factors).

prime_factors(N) -> prime_factors(N, 2, []).
prime_factors(1, _, Factors) -> lists:usort(Factors);
prime_factors(N, F, Factors) -> prime_factors(N, F, N rem F, Factors).
prime_factors(N, F, 0, Factors) -> prime_factors(N div F, F, [F | Factors]);
prime_factors(N, F, _, Factors) -> prime_factors(N, F+1, Factors).

factors_test_() ->
  [
    ?_assertEqual([1], factors(1))
   ,?_assertEqual([1,3], factors(3))
   ,?_assertEqual([1,2,3,6], factors(6))
   ,?_assertEqual([1,2,5,10], factors(10))
   ,?_assertEqual([1,3,5,15], factors(15))
   ,?_assertEqual([1,3,7,21], factors(21))
   ,?_assertEqual([1,2,4,7,14,28], factors(28))
  ].
common_factors_test_() ->
  [
    ?_assertEqual([1], common_factors({1,3}))
   ,?_assertEqual([1,3], common_factors({3,9}))
   ,?_assertEqual([1,2,3,6], common_factors({6,12}))
   ,?_assertEqual([1,2,5,10], common_factors({10,30}))
   ,?_assertEqual([1,3,5,15], common_factors({15,45}))
   ,?_assertEqual([1,3,7,21], common_factors({21,42}))
   ,?_assertEqual([1,2,4,7,14,28], common_factors({28,84}))
   ,?_assertEqual([1,2,4], common_factors({4,8}))
   ,?_assertEqual([1,2], common_factors([2,4,6]))
  ].
prime_factors_test_() ->
  [
    ?_assertEqual([2], prime_factors(4))
   ,?_assertEqual([2,3], prime_factors(12))
   ,?_assertEqual([2,5], prime_factors(12500))
  ].
