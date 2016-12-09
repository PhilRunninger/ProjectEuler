% We shall say that an n-digit number is pandigital if it makes use of all
% the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
% and is also prime.

% What is the largest n-digit pandigital prime that exists?

% An easy check is divisibility by 3 - if the sum of a number's digits is
% divisible by 3, then the number itself is too. All n-digit pandigital
% numbers have the same sum of digits, so we can check a representative
% number for each n.
%
%                     n:  1  2   3    4     5      6       7        8         9
% 1st pandigital number:  1 12 123 1234 12345 123456 1234567 12345678 123456789
%         sum of digits:  1  3   6   10    15     21      28       36        45
%       possbibly prime: no no  no  yes    no     no     yes       no        no
%
% So now we only have to check 4- and 7-digit pandigital numbers for primality,
% a big time savings by having to check for 8- and 9- digit primes.

lists:map(
  fun(N) ->
          {N, hd(
                lists:reverse(
                  lists:sort(
                    lists:filter(
                      fun(Candidate) ->
                              primes:is_prime(Candidate)
                      end,
                      lists:map(
                        fun(Digits) ->
                                lists:foldl(
                                  fun(In,Acc) ->
                                          10*Acc+In
                                  end,
                                  0, Digits)
                        end,
                        combinatorics:permutations(lists:seq(1,N,1))
                      )
                    )
                  )
                )
              )
          }
  end, [4,7]).
