% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

% One line solution in the shell: 

lists:foldl(fun(X,Sum) -> Sum + lists:foldl(fun(_, Power) -> Power*X end, 1, lists:seq(1,X)) end, 0, lists:seq(1,1000)) rem 10000000000.
