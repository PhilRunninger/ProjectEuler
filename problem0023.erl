-module(problem0023).

-compile([export_all]).

solve() ->
    Abundant = lists:filter(fun(X) -> factors:is_abundant(X) end, lists:seq(1,28123)),
    Sums = [ X+Y || X <- Abundant, Y <-Abundant, X =< Y, X+Y =< 28123],
    NotSums = lists:seq(1,28123) -- Sums,
    lists:sum(NotSums).
