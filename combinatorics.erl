-module(combinatorics).
-export([combinations/2, permutations/1, permutations/2]).

combinations(0,_) -> [[]];
combinations(_,[]) -> [];
combinations(N,[H|T]=S) -> [[H|L] || L <- combinations(N-1,S)]++combinations(N,T).

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].

permutations(L,N) ->
    lists:usort(
      lists:map(fun(I) ->
                        {A,_} = lists:split(N,I),
                        A
                end, permutations(L))).
