%====1===

flatten([],[]):-!.
flatten([X|L],FL):- !, flatten(X,NX),flatten(L,NL),append(NX,NL,FL).
flatten(X,[X]).


deleteReps(_,[],[]).
deleteReps(X,[X|L1],L):-deleteReps(X,L1,L).
deleteReps(X, [Y|L1],[Y|L]):- deleteReps(X,L1,L).

noRep([],[]).
noRep([X|L1],[X|L2]):-deleteReps(X,L1,L3), noRep(L3,L2).

flattenNoRepetitions(X,L):-flatten(X,R), noRep(R,L),!.


%====2====

isNextTo(X,Y):-X is Y-1.
isNextTo(X,Y):-X is Y+1.

%1 - El que vive en la casa roja es de Peru
%2 - Al frances le gusta el perro
%3 - El pintor es japones
%4 - Al chino le gusta el ron
%5 - El hungaro vive en la primera casa
%6 - Al de la casa verde le gusta el coñac
%7 - La casa verde esta a la izquierda de la blanca
%8 - El escultor cría caracoles
%9 - El de la casa amarilla es actor
%10 - El de la tercera casa bebe cava
%11 - El que vive al lado del actor tiene un caballo
%12 - El hungaro vive al lado de la casa azul
%13 - Al notario la gusta el whisky
%14 - El que vive al lado del medico tiene un ardilla,

%   [num_casa,color,profesion,animal,bebida,pais] 
casas:-	Sol = [	[1,A1,B1,C1,D1,E1],
                [2,A2,B2,C2,D2,E2],
                [3,A3,B3,C3,D3,E3],
                [4,A4,B4,C4,D4,E4],
                [5,A5,B5,C5,D5,E5] ],
       
        member([_,roja,_,_,_,peru] , Sol),
        member([_,_,_,perro,_,francia],Sol),
        member([_,_,pintor,_,_,japon],Sol),
        member([_,_,_,_,ron,china],Sol),
        member([1,_,_,_,_,hungria],Sol),
        member([_,verde,_,_,coñac,_],Sol),
        member([V,verde,_,_,_,_],Sol), member([V1,blanca,_,_,_,_],Sol),V1 is V+1,
        member([_,_,escultor,caracoles,_,_],Sol),  
        member([_,amarilla,actor,_,_,_],Sol),
        member([3,_,_,_,cava,_],Sol),
        member([R,_,_,caballo,_,_],Sol), member([R1,_,actor,_,_,_],Sol), isNextTo(R,R1),
        member([H,_,_,_,_,hungria],Sol), member([H1,azul,_,_,_,_],Sol), isNextTo(H,H1),
        member([_,_,notario,_,whisky,_],Sol),
        member([F,_,_,ardilla,_,_],Sol), member([F1,_,medico,_,_,_],Sol), isNextTo(F,F1),
        write(Sol), nl.
        
        
%===3====
omplir([]).
omplir([X|L]):- between(1,8,X) , omplir(L).

isSafe([]).
isSafe([X|L]):- threats(X,L,1), isSafe(L).


threats(_,[],_).
threats(X,[Y|L],F):- X \= Y, not(X is Y+F), not(X is Y-F), Fn is F+1, threats(X,L,Fn).

showTable([]):-!.
showTable([1|L]):-nl, write('x . . . . . . .'), showTable(L).
showTable([2|L]):-nl, write('. x . . . . . .'), showTable(L).
showTable([3|L]):-nl, write('. . x . . . . .'), showTable(L).
showTable([4|L]):-nl, write('. . . x . . . .'), showTable(L).
showTable([5|L]):-nl, write('. . . . x . . .'), showTable(L).
showTable([6|L]):-nl, write('. . . . . x . .'), showTable(L).
showTable([7|L]):-nl, write('. . . . . . x .'), showTable(L).
showTable([8|L]):-nl, write('. . . . . . . x'), showTable(L).

                    
reinas :- L = [X1, X2, X3, X4, X5, X6, X7, X8],
       omplir(L),
       isSafe(L),    
       showTable(L).
