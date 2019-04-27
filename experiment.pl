parent(a,b).
parent(b,c).

descendent(X, Y) :- parent(X, Y).
descendent(X, Y) :- parent(X, Z), descendent(Z, Y).
