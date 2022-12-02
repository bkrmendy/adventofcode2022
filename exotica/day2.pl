#! /opt/homebrew/bin/swipl
:- initialization main.

%% UTILS
split_along(Separator, Source, Result) :-
    split_string(Source, Separator, "", Result).

sum(S, R) :- sum(S, 0, R).
sum([], Sum, Sum).
sum([H | R], Acc, Result) :-
    Interm is Acc + H,
    sum(R, Interm, Result).

%% THE ACTUAL CHALLENGE

play(A, A, draw).

play(scissors, rock, win).
play(paper, scissors, win).
play(rock, paper, win).

play(_, _, lose).

code(C, rock) :- C = "A" ; C = "X".
code(C, paper) :- C = "B" ; C = "Y".
code(C, scissors) :- C = "C" ; C = "Z".

shape(rock, 1).
shape(paper, 2).
shape(scissors, 3).

result(win, 6).
result(draw, 3).
result(lose, 0).

outcome([A, B], Score) :-
    shape(B, Shape),
    play(A, B, Result),
    result(Result, S),
    Score is (Shape + S).

translate([A, B], [X, Y]) :- code(A, X), code(B, Y).

correct([Left, rock], [Left, Right]) :- play(Left, Right, lose).
correct([Left, paper], [Left, Right]) :- play(Left, Right, draw).
correct([Left, scissors], [Left, Right]) :- play(Left, Right, win).

parse(Input):-
    open('../input/02.txt', read, Source),
    read_string(Source, "", "", _, String),
    split_along("\n", String, Lines),
    maplist(split_along(" "), Lines, Words),
    maplist(translate, Words, Input).

part1(Challenge, Result) :-
    maplist(outcome, Challenge, Scores),
    sum(Scores, Result).

part2(Challenge, Result) :-
    maplist(correct, Challenge, Corrected),
    maplist(outcome, Corrected, Scores),
    sum(Scores, Result).

main :-
    parse(Input),
    part1(Input, Part1Result),
    part2(Input, Part2Result),
    write(Part1Result),
    write("\n"),
    write(Part2Result).