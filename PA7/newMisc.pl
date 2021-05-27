zip([],[],[]).
zip(X,Y,Z) :- X = [H1|T1], Y = [H2|T2], Z=[H3|T3], H3 = [H1,H2], zip(T1,T2,T3).

part([], X, Y, Z).
part([H|T], X, Y, Z) :- H > X, Z2 = [H|Z], part(T, X, Y, Z2).
part([H|T], X, Y, Z) :- H =< X, Y2 = [H|Y], part(T, X, Y, Y2).
