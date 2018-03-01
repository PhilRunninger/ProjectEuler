-module(factors).

-export([factors/1, common_factors/1, prime_factors/1, prime_factors/2, proper_factors/1, is_perfect/1, is_deficient/1, is_abundant/1]).

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

prime_factors(N) -> prime_factors(N, unique).
prime_factors(N, Option) -> prime_factors(N, 2, [], Option).
prime_factors(1, _, Factors, all) -> lists:sort(Factors);
prime_factors(1, _, Factors, unique) -> lists:usort(Factors);
prime_factors(1, _, Factors, with_powers) -> lists:map(fun(P) -> {P, length([Q || Q<-Factors, Q==P])} end, lists:usort(Factors));
prime_factors(N, F, Factors, Option) -> prime_factors(N, F, N rem F, Factors, Option).
prime_factors(N, F, 0, Factors, Option) -> prime_factors(N div F, F, [F | Factors], Option);
prime_factors(N, 2, _, Factors, Option) -> prime_factors(N, 3, Factors, Option);
prime_factors(N, F, _, Factors, Option) -> prime_factors(N, F+2, Factors, Option).

proper_factors(N) ->
    [ X || X <- factors(N), X < N ].

is_perfect(N) -> lists:sum(proper_factors(N)) == N.
is_deficient(N) -> lists:sum(proper_factors(N)) < N.
is_abundant(N) -> lists:sum(proper_factors(N)) > N.

factors_test_() ->
    lists:map(fun({Desc, Input, Expected}) ->
                      {Desc, ?_assertEqual(Expected, factors(Input))}
              end,
              [
               {<<"one">>, 1, [1]}
              ,{<<"three">>, 3, [1,3]}
              ,{<<"six">>, 6, [1,2,3,6]}
              ,{<<"ten">>, 10, [1,2,5,10]}
              ,{<<"fifteen">>, 15, [1,3,5,15]}
              ,{<<"twenty-one">>, 21, [1,3,7,21]}
              ,{<<"twenty-eight">>, 28, [1,2,4,7,14,28]}
              ]).
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
   ,?_assertEqual([2,5], prime_factors(12500, unique))
   ,?_assertEqual([2,2,3], prime_factors(12, all))
   ,?_assertEqual([2,2,5,5,5,5,5], prime_factors(12500, all))
   ,?_assertEqual([{2,2}], prime_factors(4, with_powers))
   ,?_assertEqual([{2,2},{3,1}], prime_factors(12, with_powers))
   ,?_assertEqual([{2,2},{5,5}], prime_factors(12500, with_powers))
  ].
proper_factors_test_() ->
    [
     ?_assertEqual([1,2,3], proper_factors(6))
    ,?_assertEqual([1,2,3,4,6], proper_factors(12))
    ].
is_perfect_test_() ->
    [
     ?_assertEqual(false, is_perfect(5))
    ,?_assertEqual(true, is_perfect(6))
    ,?_assertEqual(false, is_perfect(12))
    ].
is_deficient_test_() ->
    [
     ?_assertEqual(true, is_deficient(5))
    ,?_assertEqual(false, is_deficient(6))
    ,?_assertEqual(false, is_deficient(12))
    ].
is_abundant_test_() ->
    [
     ?_assertEqual(false, is_abundant(5))
    ,?_assertEqual(false, is_abundant(6))
    ,?_assertEqual(true, is_abundant(12))
    ].
