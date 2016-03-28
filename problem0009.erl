% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
%
% a^2 + b^2 = c^2
% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
%
% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.

-module(problem0009).
-export([solve/0, solve/1]).

% Instead of brute-forcing this, do a little algebra first to simplify things.
%     Let s be the perimeter of the triangle.
%     Let a, b be the legs of the triangle.
%     Let c be the hypotenuse of the triangle.
%
%                           a + b + c = s                                               | (1)
%                           c = s - a - b                                               | (2)
%                           a^2 + b^2 = c^2                                             | (3)
% Substitute (2) into (3).
%                           a^2 + b^2 = (s - a - b)^2                                   | (4)
% Expand.
%                           a^2 + b^2 = s^2 - sa - sb - sa + a^2 + ab - sb + ab + b^2   | (5)
% Simplify.
%                           0 = s^2 - 2sa - 2sb + 2ab                                   | (6)
% Solve for b.
%                           2b(s - a) = s^2 - 2sa                                       | (7)
%                           b = (s^2 - 2sa) / 2(s - a)                                  | (8)
%
% Now we have a much smaller space in which to search.
%   For each a from 1 to S/2
%     calculate b
%     if b is an integer...
%       calculate c
%       multiply a*b*c.
%       we're done.

solve() ->
  solve(1000).
solve(S) ->
  [{A, round(B), round(math:sqrt(A*A+B*B)), round(A*B*math:sqrt(A*A+B*B))} || A <- lists:seq(1,S div 2), B <- [(S*S - 2*A*S)/(2*(S-A))], A<B, B == round(B)].
