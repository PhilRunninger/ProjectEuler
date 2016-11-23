%%% vim:foldmethod=marker
-module(factorial).

-export([compute/1]).

-include_lib("eunit/include/eunit.hrl").

%%% API   {{{1

compute(N) ->
    compute(N, 1).

compute(N, _) when N < 0 -> error;
compute(0, Result) -> Result;
compute(N, Result) -> compute(N-1, Result*N).

%%% Internal functions   {{{1
%%% Unit Tests   {{{1
-ifdef(EUNIT).

fact_test_() ->
    [
     ?_assertEqual(1, compute(0)),
     ?_assertEqual(1, compute(1)),
     ?_assertEqual(2, compute(2)),
     ?_assertEqual(6, compute(3)),
     ?_assertEqual(error, compute(-1)),
     ?_assertEqual(error, compute(3.14))
    ].

-endif.
