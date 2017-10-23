% This was done manually in the Erlang shell.

All = combinatorics:permutations([0,1,2,3,4,5,6,7,8,9]).
All2  = lists:filter(fun([_,H,T,O|_]) -> O rem 2 =:= 0 end, All).
All3  = lists:filter(fun([_,_,H,T,O|_]) -> (100*H+10*T+O) rem 3 =:= 0 end, All2).
All5  = lists:filter(fun([_,_,_,H,T,O|_]) -> (100*H+10*T+O) rem 5 =:= 0 end, All3).
All7  = lists:filter(fun([_,_,_,_,H,T,O|_]) -> (100*H+10*T+O) rem 7 =:= 0 end, All5).
All11 = lists:filter(fun([_,_,_,_,_,H,T,O|_]) -> (100*H+10*T+O) rem 11 =:= 0 end, All7).
All13 = lists:filter(fun([_,_,_,_,_,_,H,T,O|_]) -> (100*H+10*T+O) rem 13 =:= 0 end, All11).
All17 = lists:filter(fun([_,_,_,_,_,_,_,H,T,O|_]) -> (100*H+10*T+O) rem 17 =:= 0 end, All13).
[[1,4,0,6,3,5,7,2,8,9],
 [1,4,3,0,9,5,2,8,6,7],
 [1,4,6,0,3,5,7,2,8,9],
 [4,1,0,6,3,5,7,2,8,9],
 [4,1,3,0,9,5,2,8,6,7],
 [4,1,6,0,3,5,7,2,8,9]]
lists:sum(lists:map(fun(L) -> lists:foldl(fun(X,Sum) -> 10*Sum+X end, 0, L) end, All17)).
16695334890
