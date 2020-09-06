%=====Exercicis=====

%==========2=============

prod([X|L],P):- prod(L,P1), P is P1*X.
prod([],1).

%==========3=============

pescalar([X|L1],[Y|L2],P):- !,pescalar(L1,L2,P1), P is X*Y+P1.
pescalar([],[],0).

%==========4=============

union([],L,L).
union([X|L1],L2,L3):- member(X,L2),!,union(L1,L2,L3).
union([X|L1],L2,[X|L3]):- union(L1,L2,L3).

interseccion([],_,[]).
interseccion([X|L1],L2,L3):- not(member(X,L2)),!, interseccion(L1,L2,L3).
interseccion([X|L1],L2,[X|L3]):- member(X,L2),!, interseccion(L1,L2,L3).

%==========5=============
%---------
%Devido a que el concat es para atomos,
%y no puedo redefinirlo como en la lista de ejercicios usare append
%---------

ultimo(L,X):-append(_,[X],L),!.

inverso([],[]).
inverso([X|L],LI):-!,inverso(L,I2), append(I2,[X],LI).

%==========6=============

fib(1,1):-!.
fib(2,1):-!.
fib(N,F):- N>2,N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.

%==========7=============

dados(0,0,[]).
dados(P,N,[X|L]):- N>0, between(1,6,X), P2 is P-X, N2 is N-1, dados(P2,N2,L).  

%==========8=============

sumatotal([S],S):-!.
sumatotal([X|L],N):- sumatotal(L,N2), N is N2+X.

elemento_es_2N([],0):-!.
elemento_es_2N([X|_],N):- N is X+X,!.
elemento_es_2N([X|L],N):- not(N is X+X), elemento_es_2N(L,N).

suma_demas(L):-sumatotal(L,N), elemento_es_2N(L,N),!.

%==========9=============

suma_ants(L):- append(Head, [X|_],L), sumatotal(Head,X),!. 

%==========10============

conta(_,[],1).
conta(X,[X|L],R):- !,conta(X,L,R2), R is R2+1.
conta(X,[_|L],R):- !,conta(X,L,R).

deleteReps(_,[],[]).
deleteReps(X,[X|L1],L):-deleteReps(X,L1,L).
deleteReps(X, [Y|L1],[Y|L]):- deleteReps(X,L1,L).

card([],[]).
card([X|L],[[X,R]|L2]):-conta(X,L,R), deleteReps(X,L,L3),!,card(L3,L2).

card(L):- card(L,L2),write(L2). 

%==========11============

esta_ordenada([]):-!.
esta_ordenada([_]):-!.
esta_ordenada([X,Y|L]):- X =< Y, esta_ordenada([Y|L]),!.

%========UTILS===========

pert_con_resto(X,L,Resto):- append(L1,[X|L2],L), append(L1,L2,Resto).
permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

%==========12============

ordenacion([],[]).
ordenacion(L1,L2):-permutacion(L1,L2),esta_ordenada(L2),!.

%==========13============
% El caso peor es cuando se realizan todas las permutaciones posibles
% en ese caso por una lista de n elementos existen n! permutaciones
% esta ordenada realiza n-1 comparaciones (el último elemento no tiene siguiente con el que compararlo).
% Por lo tanto se realizan n!*(n-1) comparaciones en el peor caso

%==========14============

insercion(X,[],[X]). 
insercion(X,[Y|L],[X,Y|L]) :- X=<Y. 
insercion(X,[Y|L],[Y|L1]) :- insercion(X,L,L1).

ordenacion_ins([],[]). 
ordenacion_ins([X|L1],L2) :- ordenacion_ins(L1,L3), insercion(X,L3,L2),!.

%==========15============

% En el peor caso la insercion tiene que ir en la última posicion por lo tanto
% en una lista de n elementos se haran n comparaciones.
% Al hacer la ordenacion se añade primero un elemento a una lista vacia, y se van añadiendo
% asi que se haran sum(0,n-1) comparaciones. Es decir  n*(n-1)/2 comparaciones.

%==========16============
merge(L,[],L).
merge([],L,L).
merge([X|L1],[Y|L2],[X|L3]):- X =< Y,!, merge(L1,[Y|L2],L3).
merge([X|L1],[Y|L2],[Y|L3]):- X > Y,!, merge([X|L1],L2,L3).
 

ord_merge([],[]).
ord_merge([A],[A]).
ord_merge(L1,L2):- append(X,Y,L1), length(X,LX), length(Y,LY),
 LX is LY,ord_merge(X,X1), ord_merge(Y,Y1), merge(X1,Y1,L2),!.

ord_merge(L1,L2):- append(X,Y,L1), length(X,LX), length(Y,LY),
 LX is LY+1,ord_merge(X,X1), ord_merge(Y,Y1), merge(X1,Y1,L2),!. 
 
%==========17============


diccionario(A,N):- !,nmember(A,N,L), escriu(L).

nmember(_,0,[]):-!.
nmember(A,N,[X|L]):- member(X,A), N2 is N-1, nmember(A,N2,L).  

escriu([]):-write(' '),fail.
escriu([X|L]):-write(X), escriu(L).

%==========18============

palindromos(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S). 

es_palindromo([]).
es_palindromo([_]) :- !. 
es_palindromo([X|L]) :- concat(L1,[X],L), es_palindromo(L1). 

%==========19============


suma([],[],N,[N|_]):-!.
suma([X|L1],[Y|L2],N,[R|Res]):-R1 is X+Y, R is R1+N, R < 10, suma(L1,L2,0,Res).
suma([X|L1],[Y|L2],N,[R|Res]):-R1 is X+Y, R2 is R1+N, R2 >= 10, R is R2 mod 10, suma(L1,L2,1,Res).

equals([],_):-!.
equals([X|L1],[Y|L2]):- X is Y, equals(L1,L2).

send_money:-
        L = [S, E, N, D, M, O, R, Y, _, _],
	permutation(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D,N,E,S],[E,R,O,M],0,X),
	equals([Y,E,N,O,M],X),
	write('es'), nl,
	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.
	
send_more_money:-
		not(send_money), write('no'),!.


%==========20============


operacion(A + B, C):- number(A), number(B), !, C is A + B.
operacion(A - B, C):- number(A), number(B), !, C is A - B.
operacion(A * B, C):- number(A), number(B), !, C is A * B.
operacion(0 * A, 0):- atomic(A), !.
operacion(A * 0, 0):- atomic(A), !.
operacion(A * 1, A):- !.
operacion(1 * A, A):- !.
operacion(_ / 0, _):- fail,!.
operacion(0 / _, 0):-!.
operacion(A / B, C):- number(A), number(B),!, C is A / B.
operacion(A*x / B*x, C):-A2 is A/B, A2>1, operacion(A2*x/x,C),!.
operacion(A*x / B*x, C):-A2 is B/A, A2>=1, operacion(x/A2*x,C),!.
operacion(A*x / x*B,C):- operacion(A*x/B*x,C),!.
operacion(x * A, C):- operacion(A * x, C), !.
operacion(A * (x + B),C):- A2 is A*B, operacion(A*x + A2,C),!.
operacion(A * (x - B),C):- A2 is A*B, operacion(A*x - A2,C),!.
operacion(A*x + B*x , C):- A2 is A+B, operacion(A2*x,C),!.
operacion(A, A).

simplifica(A + B, C) :- !, simplifica(A, A1), simplifica(B, B1), operacion(A1 + B1, C).
simplifica(A - B, C) :- !, simplifica(A, A1), simplifica(B, B1), operacion(A1 - B1, C).
simplifica(A * B, C) :- !, simplifica(A, A1), simplifica(B, B1), operacion(A1 * B1, C).
simplifica(A / B, C) :- !, simplifica(A, A1), simplifica(B, B1), operacion(A1 / B1, C).
simplifica(A, A).

%==========21============

camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
	unPaso(EstadoActual, EstadoSiguiente),
	\+member(EstadoSiguiente,CaminoHastaAhora),
	camino(EstadoSiguiente,EstadoFinal,[EstadoSiguiente|CaminoHastaAhora],CaminoTotal).
	
missionary(3).
cannibal(3).
canoe(2).

isSafe(0,_).
isSafe(M,C):-
	M >= C.

% *Mra ~ Missionaries Right_side After...*
% canoe from left to right
unPaso([l,Mlb,Clb,Mrb,Crb],[r,Mla,Cla,Mra,Cra]):-
	canoe(Kmax),
	between(0,Mlb,Km),
	between(0,Clb,Kc),

	K is Km + Kc, 		% canoe passengers are the sum of M and C picked from left
	K >= 1,				% check canoe passengers capacity
	K =< Kmax,
	%% ensure nextState constraints
	Mla is Mlb - Km,
	Cla is Clb - Kc,
	isSafe(Mla,Cla),
	Mra is Mrb + Km,
	Cra is Crb + Kc,
	isSafe(Mra, Cra).

unPaso([r,Mlb,Clb,Mrb,Crb],[l,Mla,Cla,Mra,Cra]):-
	canoe(Kmax),
	between(0,Mrb,Km),
	between(0,Crb,Kc),

	K is Km + Kc, 		% canoe passengers are the sum of M and C picked from left
	K >= 1,				% check canoe passengers capacity
	K =< Kmax,
	%% ensure nextState constraints
	Mra is Mrb - Km,
	Cra is Crb - Kc,
	isSafe(Mra,Cra),
	Mla is Mlb + Km,
	Cla is Clb + Kc,
	isSafe(Mla,Cla).

solucionOptima:-
	missionary(M),
	cannibal(C),
	camino([l,M,C,0,0],[r,0,0,M,C],[[l,M,C,0,0]],Ca),
	write(Ca), nl.
