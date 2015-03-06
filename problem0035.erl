% The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
% There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
% How many circular primes are there below one million?

-module(problem0035).
-export([solve/1]).
-include_lib("eunit/include/eunit.hrl").

solve(Max) ->
    Primes = primes:less_than(Max),
    length(lists:filter(fun(Prime) ->
                                lists:all(fun(N) -> 
                                                  lists:member(N, Primes)
                                          end,
                                          rotate_number(Prime))
                        end,
                        Primes)).

rotate_number(N) ->
    rotate_number(trunc(math:log10(N))+1, integer_to_list(N), []).

rotate_number(0, _N, List) -> List;
rotate_number(Iteration, [H|T], List) ->
    NewNumber = T ++ [H],
    rotate_number(Iteration - 1, NewNumber, [list_to_integer(NewNumber) | List]).

rotate_number_test_() ->
    [?_assertEqual([197,719,971], rotate_number(197))
    ,?_assertEqual([7], rotate_number(7))
    ].
solve_test_() ->
    [?_assertEqual(13, solve(100))
    ,?_assertEqual(9, solve(72))
    ].
