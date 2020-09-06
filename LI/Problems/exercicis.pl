pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L),!.

prod([],1).
prod([X|XS],P):-
	prod(XS,P1),
	P is X*P1.


pescalar([],[],0).
pescalar([X|XS],[Y|YS],P):-
	pescalar(XS,YS,P1),
	P is (X*Y)+P1.
	
union([],L,L).
union([X|L1],L2,L):-
	pert(X,L2),!,
	union(L1,L2,L).
union([X|L1],L2,[X|L]):-
	union(L1,L2,L).
	
interseccion([],_,[]).
interseccion([X|L1], L2, [X|L3]):-
		pert(X,L2),!,
		interseccion(L1,L2,L3).
interseccion([_|L1], L2, L3):-
	interseccion(L1, L2, L3).

concat([],L,L).
concat([X|XS], L2, [X|L]) :- concat(XS,L2,L). 	

ultimo(L, R) :- concat(_,[R],L).			% ),!.

inverso([],[]).
inverso(L, [X|L1]) :- concat(L2,[X], L), inverso(L1,L2). 
	
fib(1,1):-!.
fib(2,1):-!.
fib(N,R):- 
	A is N-1,
	B is N-2,
	fib(A,R1), 
	fib(B,R2), 
	R is R1+R2. 


dados(0,0,[]).
dados(P,N,[X|XS]):-						%P puntos N dados
	N > 0,
	pert(X,[1,2,3,4,5,6]),
	Q is P-X,
	N2 is N-1,
	dados(Q,N2,XS).
	

suma([],0).
suma([X|XS], R) :-
	suma(XS,R2),
	R is X+R2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%EXAMEN ALTRE GRUP%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subcjto([],[]).  %subcjto(L,S) significa "S es un subconjunto de L".
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).


%subset_rest(L,S,R):-
%	subcjto(L,S),
%	concat(R,S,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
treu_x(X,L,R):- concat(L1,[X|L2],L), concat(L1, L2, R).
pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).  
%%%%%%%%

suma_demas(L,X):- treu_x(X,L,L2), suma(L2,X), !. %al afegir ! parara al trobar el primer
suma_demas(L):- suma_demas(L,X), write(X).

suma_ants(L,X):- concat(L1,[X|_], L), suma(L1, X), !.
suma_ants(L):-suma_ants(L,X),write(X).


llargada([],0).
llargada([_|L],M):- llargada(L, N), M is N+1.


card([],[]):-!.
card( [X|L] , [ [X,N1] | Cr]):- card(L,C), treu_x([X,N], C, Cr),!,N1 is N+1.
card( [X|L], [ [X,1] | Cr]):- card(L,Cr).

card(L):- card(L,R), write(R).


esta_ordenada([]).
esta_ordenada([_]) :- !.
esta_ordenada([X,Y|L]) :- X=<Y, esta_ordenada([Y|L]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
llistaEq([],[]):-!.
llistaEq([X|XS],L):- pert(X,L), treu_x(X,L,R),llistaEq(XS,R).		% primer parametre es igual al segon desordenat o no


permutacion([],[]).
permutacion(L,[X|P]) :- treu_x(X,L,R), permutacion(R,P).		% segon parametre es igual al primer desordenat o no
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



ordenacion(L1,L2):- permutacion(L1,L2), esta_ordenada(L2),!.		% L2 es L1 ordenat

ordenacion_i([], L1,L2):- ordenacion(L1,L2).				% L2 es L1 afegint X i ordenat. 
ordenacion_i(X,L1,L2):- union([X],L1,L), ordenacion(L,L2).



%%%%%%%%%%%%%%%%%%%%%%%%% MERGESORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split([],[],[]).
split([X],[],[X]).
split([A,B|XS],[A|AS],[B|BS]):- split(XS,AS,BS).

ordenacion_ms([],[]):-!.
ordenacion_ms([X],[X]):-!.
ordenacion_ms(L,R):- split(L,L1,L2),
		 ordenacion_ms(L1,R1), ordenacion_ms(L2,R2),
		 merge(R1,R2,R).

merge(L,[],L):-!.
merge([],L,L).
merge([X|L1],[Y|L2],[X|L3]) :- X =< Y,!, merge(L1,[Y|L2],L3).
merge([X|L1],[Y|L2],[Y|L3]) :-  merge([X|L1],L2,L3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pues no ha servio de nah 
ordenaSimbol(A,_,[A|_], A):-!.
ordenaSimbol(_,B,[B|_],B):-!.
ordenaSimbol(A,B,[_|LS],R):- ordenaSimbol(A,B,LS,R).

%%%%% diccionari%%%%%%%%%%
nperts(_,0,[]):-!.
nperts(L,N,[A|AS]):- pert(A,L), N1 is N-1, nperts(L, N1, AS).

diccionario(A,N):- nperts(A,N,S), escribir(S), fail. % el fail forsa el backtraking a que no saturi al satisfer la primera paraula

escribir([]):- write(' '),!.
escribir([S|SS]):- write(S), escribir(SS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
es_palindromo([]).
es_palindromo([_]):-!.
es_palindromo([X|XS]):-concat(L1,[X],XS), es_palindromo(L1).



%palindromos(L):-  permutacion(L,S), es_palindromo(S), write(S), nl,fail.
%sense escriure repetits
palindromos(L):- setof(S,(permutacion(L,S), es_palindromo(S)),R),write(R).

%%%%%%%%%%%%%%%%%% 19 LOCURA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



mis:- camino( [lado1,3,3], [lado2,0,0], [[lado1,3,3]] ).

camino(Fin,Fin,Cam):- inverso(Cam,Sol), write(Sol), nl.
camino(Ini,Fin,Cam):- paso(Ini,E), novisitado(E,Cam), camino(E,Fin,[E|Cam]).

novisitado(E,Cam):- \+pert(E,Cam), !,fail.
novisitado(_,_).

paso( [lado1,M1,C1], [lado2,M2,C2] ):- pasan(M,C), M2 is M1-M, C2 is C1-C, safe(M2,C2).
paso( [lado2,M1,C1], [lado1,M2,C2] ):- pasan(M,C), M2 is M1+M, C2 is C1+C, safe(M2,C2).

pasan(M,C):- member( [M,C], [ [0,1], [0,2], [1,0], [1,1], [2,0] ] ).

safe(M,C):- M>=0, M=<3, C>=0, C=<3, nocomen( M, C),
            M1 is 3-M,  C1 is 3-C,  nocomen(M1,C1).

nocomen(0,_).
nocomen(M,C):- M>=C.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Escribe un predicado prolog "flatten" que aplana listas:
% ?- flatten( [a,[b,c,[b,b],e], f], L).
% L = [a,b,c,b,b,e,f]
% Escribe otro que elimina las repeticiones:
% ?- flattenNoRepetitions([a,[b,c,[b,b],e], f], L).
% L = [a,b,c,e,f] */

flatten([],[]):-!. 
flatten([L|LS], RS) :- !, flatten(L,LR), concat(LR,XS,RS), flatten(LS,XS).
flatten(X,[X]).

flattenNoRepetitions(L, R):- flatten(L,P), noR(P,R).

noR([],[]):-!.
noR([X|LS],[X|RS]):- deleteReps(X,LS,R2), noR(R2, RS). 

deleteReps(_,[],[]):-!.
deleteReps(X,[X|LS],R):- deleteReps(X,LS,R),!.
deleteReps(X,[N|LS],R):- deleteReps(X,LS,RS), concat([N],RS,R).


%%%%%%%%%%%%%%%%%%%%%%%%%MOSTRA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%1.- [2 points] Write a Prolog predicate subset_rest(L,S,R) that
%succeeds if S is a subset of L, and R are the remaining elements. It
%should be able to compute all possible answers upon
%backtrack. Example:

%?- subset_rest([1,2,3],S,R), write(S), nl, write(R), nl, nl, fail.
subset_rest([],[],[]):-!.
subset_rest(L,S,R):- subcjto(L,S), ordenacion(L,LO), append(S,R,LC), ordenacion(LC,LO).




%2.- [3 points] We want to determine whether a graph G = (V,E) is
%bipartite.  That is, if there exists a partition of its vertices
%V = S1 U S2 such that there is no edge among vertices of S1 or among
%vertices of S2.  Implement a Prolog predicate bipartite(N,L,S1,S2),
%that succeeds if the graph with vertices {1,2,...,N} and edges E is
%bipartite with partition S1 U S2. Example:

%?- bipartite(5,[[1,3],[2,3],[2,4],[3,5],[4,5]],S1,S2).

%S1 = [1, 2, 5],
%S2 = [3, 4].	

vN(Ni,Ni,[Ni]).
vN(Ni,Nf,[Ni|LS]):- N1 is Ni+1, vN(N1,Nf,LS),!.



bipartite(N,L,S1,S2):- vN(1,N,V), subcjto(V,S1), subcjto(V,S2), capCon(L,S1), capCon(L,S2), append(S1,S2,S),  ordenacion(S,V),!.

capCon(_,[]):-!.
capCon(_,[_]):-!.
capCon(L,[S|SS]):- pert(X,[S|SS]), pert(Y,[S|SS]), X < Y, \+pert([X,Y],L), capCon(L,SS),!.
  


