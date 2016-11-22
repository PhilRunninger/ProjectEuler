% https://projecteuler.net/problem=33
%
% The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
% We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
% There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
% If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

% The fractions can be expressed with the digits xy/zx or yx/xz, which by "cancellation", reduce to y/z.
% Set (10x+y)/(10z+x) = y/z   (set 1), and
%     (10y+x)/(10x+z) = y/z   (set 2).
% Cross multiply to get the constraints 10xz+yz = 10yz+xy (set 1), and
%                                       10yz+xz = 10xy+yz.

D = lists:seq(0,9,1).
N = lists:seq(1,9,1).
[{10*X+Y, 10*Z+X} || X<-N, Y<-D, Z<-N, 10*Z*X+Y*Z == 10*Z*Y+X*Y, 10*X+Y<10*Z+X]++[{10*Y+X, 10*X+Z} || X<-N, Y<-N, Z<-D, 10*Z*Y+X*Z == 10*X*Y+Z*Y, 10*Y+X<10*X+Z].

%  [{16,64},{26,65},{19,95},{49,98}]
