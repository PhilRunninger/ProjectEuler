-module(problem0022).
-export([solve/0]).

solve() ->
  {ok, Names} = file:read_file("problem0022_names.txt"),
  NameList = string:tokens(lists:flatten(re:replace(Names,"\"","",[global,{return,list}])),","),
  {_,Answer}=lists:foldl(fun(Name,{I,Sum}) -> {I+1, Sum+I*score(Name)} end, {1,0}, lists:sort(NameList)),
  Answer.

score(Name) ->
  lists:foldl(fun(X,Sum) -> X+Sum-64 end, 0, Name).
