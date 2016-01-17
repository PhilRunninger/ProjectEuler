-module(combinatorics).
-export([combinations/2, permutations/1]).

combinations(0,_) -> [[]];
combinations(_,[]) -> [];
combinations(N,[H|T]=S) -> [[H|L] || L <- combinations(N-1,S)]++combinations(N,T).

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].

