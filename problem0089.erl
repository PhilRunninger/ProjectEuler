% For a number written in Roman numerals to be considered valid there are basic rules which must be followed. Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.
%
% For example, it would appear that there are at least six ways of writing the number sixteen:
%
% IIIIIIIIIIIIIIII
% VIIIIIIIIIII
% VVIIIIII
% XIIIIII
% VVVI
% XVI
%
% However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most efficient, as it uses the least number of numerals.
%
% The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals; see About... Roman Numerals for the definitive rules for this problem.
%
% Find the number of characters saved by writing each of these in their minimal form.
%
% Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.

-module(problem0089).
-export([solve/0, to_arabic/1, to_roman/1]).
-include_lib("eunit/include/eunit.hrl").

solve() ->
    Lines = for_each_line("p089_roman.txt",
                         fun(X, List) -> [string:strip(X, right, $\n) | List] end,
                         [read],
                         []),
    lists:foldl(fun(X, Accum) ->
                        io:format("~p   =   ~p   =   ~p: ~p~n", [X, to_arabic(X), to_roman(to_arabic(X)), length(X) - length(to_roman(to_arabic(X)))]),
                        Accum + length(X) - length(to_roman(to_arabic(X)))
                end, 0, Lines).

for_each_line(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device),
                Accum;
        Line -> for_each_line(Device, Proc, Proc(Line, Accum))
    end.

to_arabic(Roman)               -> to_arabic(Roman, 0).
to_arabic([],          Arabic) -> Arabic;
to_arabic([$I|[$V|T]], Arabic) -> to_arabic(T, Arabic+4);
to_arabic([$I|[$X|T]], Arabic) -> to_arabic(T, Arabic+9);
to_arabic([$I|T],      Arabic) -> to_arabic(T, Arabic+1);
to_arabic([$V|T],      Arabic) -> to_arabic(T, Arabic+5);
to_arabic([$X|[$L|T]], Arabic) -> to_arabic(T, Arabic+40);
to_arabic([$X|[$C|T]], Arabic) -> to_arabic(T, Arabic+90);
to_arabic([$X|T],      Arabic) -> to_arabic(T, Arabic+10);
to_arabic([$L|T],      Arabic) -> to_arabic(T, Arabic+50);
to_arabic([$C|[$D|T]], Arabic) -> to_arabic(T, Arabic+400);
to_arabic([$C|[$M|T]], Arabic) -> to_arabic(T, Arabic+900);
to_arabic([$C|T],      Arabic) -> to_arabic(T, Arabic+100);
to_arabic([$D|T],      Arabic) -> to_arabic(T, Arabic+500);
to_arabic([$M|T],      Arabic) -> to_arabic(T, Arabic+1000).

to_roman(Arabic) -> to_roman([{1000,"M"}, {900,"CM"}, {500,"D"}, {400,"CD"}, {100,"C"}, {90,"XC"}, {50,"L"}, {40,"XL"}, {10,"X"}, {9,"IX"}, {5,"V"}, {4,"IV"}, {1,"I"}], Arabic, "").

to_roman(_, 0, Roman) -> Roman;
to_roman([{Max,Letter}|_T]=Translations, Arabic, Roman) when Arabic >= Max ->
    to_roman(Translations, Arabic-Max, Roman++Letter);
to_roman([_H|T], Arabic, Roman) ->
    to_roman(T, Arabic, Roman).

to_arabic_test_() ->
    [?_assertEqual(1, to_arabic("I"))
    ,?_assertEqual(5, to_arabic("V"))
    ,?_assertEqual(10, to_arabic("X"))
    ,?_assertEqual(50, to_arabic("L"))
    ,?_assertEqual(100, to_arabic("C"))
    ,?_assertEqual(500, to_arabic("D"))
    ,?_assertEqual(1000, to_arabic("M"))
    ,?_assertEqual(2, to_arabic("II"))
    ,?_assertEqual(4, to_arabic("IV"))
    ,?_assertEqual(9, to_arabic("IX"))
    ,?_assertEqual(40, to_arabic("XL"))
    ,?_assertEqual(90, to_arabic("XC"))
    ,?_assertEqual(400, to_arabic("CD"))
    ,?_assertEqual(900, to_arabic("CM"))
    ,?_assertEqual(1967, to_arabic("MCMLXVII"))
    ,?_assertEqual(49, to_arabic("XXXXVIIII"))
    ].
to_roman_test_() ->
    [?_assertEqual("I", to_roman(1))
    ,?_assertEqual("II", to_roman(2))
    ,?_assertEqual("V", to_roman(5))
    ,?_assertEqual("X", to_roman(10))
    ,?_assertEqual("L", to_roman(50))
    ,?_assertEqual("C", to_roman(100))
    ,?_assertEqual("D", to_roman(500))
    ,?_assertEqual("M", to_roman(1000))
    ,?_assertEqual("III", to_roman(3))
    ,?_assertEqual("IV", to_roman(4))
    ,?_assertEqual("IX", to_roman(9))
    ,?_assertEqual("XL", to_roman(40))
    ,?_assertEqual("XC", to_roman(90))
    ,?_assertEqual("CD", to_roman(400))
    ,?_assertEqual("CM", to_roman(900))
    ,?_assertEqual("MMMMMMMMMM", to_roman(10000))
    ,?_assertEqual("MCMLXVII", to_roman(1967))
    ,?_assertEqual("MCMLXIX", to_roman(1969))
    ].
