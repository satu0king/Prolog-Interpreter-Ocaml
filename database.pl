fact1(a).
fact1(b).
fact1(c).
fact2(a, b).
fact2(b, c).
fact2(c, d).
fact2(d, e).
fact2(e, f).
fact3(X, Y):- fact2(X,Z), fact2(Z, Y).
fact4(X, Y):- fact2(X, Y).
fact4(X, Y):- fact2(X,Z), fact4(Z, Y).
