%%%%% Add here the predicates.
%%%%% Autor: Mardoqueu Souza Vieira.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exercise 1
% Implementar o predicado lista_para_conjunto(Xs, Cs) em que Cs é uma lista que
% contém os mesmos elementos de Xs, na mesa ordem de sua primeira ocorrência, mas cujo
% número de ocorrências é apenas 1.

% Implement the predicate lista_para_conjunto (Xs, Cs) where Cs is a list that contains
% the same elements as Xs, in the order of its first occurrence, but whose number of occurrences is only 1.

%lista_para_conjunto([1,a,3,3,a,1,4], Cs).
% lista_para_conjunto(Lista, Lista_sem_duplicatas).
lista_para_conjunto([],[]).
lista_para_conjunto([H|T],Lista_sem_duplicatas):-
    remover_duplicadas(H,T,T1),
    !,
    lista_para_conjunto(T1,Tnd),
    Lista_sem_duplicatas = [H|Tnd].

% remover_duplicadas(X,L,Lr) remove todas as ocorrencias de X na Lista L e armazena o resultado na lista Lr
% removes all occurrences of X in List L and stores the result in List Lr
remover_duplicadas(_,[],[]).

remover_duplicadas(X,[X|T],Lx):-
    remover_duplicadas(X,T,Lx).

remover_duplicadas(X,[Y|T],Lx):-
    remover_duplicadas(X,T,Lx1),
    !,
    Lx=[Y|Lx1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exercise 2
% Implementar o predicado mesmo_conjunto(Cs,Ds) que sucede quando Cs e Ds são listas
% conjunto que representam os mesmo conjunto, ou seja, possuem os mesmos elementos
% ocorrendo uma única vez, em qualquer ordem. A consulta
% ?- lista_para_conjunto([1,a,3,4], Cs)
% deve suceder quando Cs é uma lista que contem qualquer permutação dos elementos
% a,1,3,4.

% Implement the same-set predicate (Cs, Ds) that happens when Cs and Ds are set lists that represent the same set,
% that is, they have the same elements occurring only once, in any order. The consultation
% ? - same_set ([1, a, 3,4], Cs)
% must occur when Cs is a list containing any permutation of elements a, 1,3,4.

addLista([], X, X).
addLista([T|H], X, [T|L]) :- addLista(H, X, L).

%Se a lista for vazia, então o resultado é também lista vazia.
mesmo_conjunto([], []).
%Se a lista tiver um elemento para a recursao.
mesmo_conjunto([X], [X]) :-!.
mesmo_conjunto([T|H], X) :-
    mesmo_conjunto(H, H1),
    addLista(L1, L2, H1),
    addLista(L1, [T], X1),
    addLista(X1, L2, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Exercise 3
%Implementar o predicado uniao_conjunto(Cs,Ds,Es) em que Cs, Ds, Es são listasconjuntos
%em que o terceiro representa a união dos dois primeiros.

% Implement the predicate uniao_conjunto(Cs, Ds, Es) where Cs, Ds, Es are joint-lists (listasconjuntos)
% where the third represents the union of the first two.


% concatenar listas (Lsta A, Lista B, concatenacao).

concat_listas( [], [H|T] , [H|T] ).

concat_listas( [H|T], [] , [H|T] ).

concat_listas( [H|T], set2 , concatenacao ):-
    concat_listas( T, set2 , concatenacao ).

concat_listas( [H|T], set2 , [H|concatenacao ):-
    concat_listas( T, set2 , concatenacao ).


uniao_conjunto([H|T],[],[H|T]).

uniao_conjunto([],[H|T],[H|T]).

uniao_conjunto([H|T], SET2, RESULT) :-
    member(H,SET2),
    uniao_conjunto(T,SET2,RESULT).

uniao_conjunto([H|T], SET2, [H|RESULT]) :-
    not(member(H,SET2)),
    uniao_conjunto(T,SET2,RESULT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exercise 4
% Implementar o predicado inter_conjunto(Cs,Ds,Es) em que Cs, Ds, Es são listasconjuntos
% em que o terceiro representa a intersecção dos dois primeiros.

% Implement the predicate inter_conjunto(Cs, Ds, Es) where Cs, Ds, Es are set-lists (listasconjuntos)
% where the third represents the intersection of the first two.

inter_conjunto([], _, []).

inter_conjunto([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    !,
    inter_conjunto(T1, L2, Res).

inter_conjunto([_|T1], L2, Res) :-
    inter_conjunto(T1, L2, Res).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exercise 5
% Implementar o predicado diferenca_conjunto(Cs,Ds,Es) em que Cs, Ds, Es são listasconjuntos
% em que o terceiro representa o conjunto diferença do primeiro, excluindo-se os
% membros do segundo.

% Implement the predicate diferenca_conjunto(Cs, Ds, Es) where Cs, Ds, Es are set-lists (listasconjuntos)
% where the third represents the difference set of the first, excluding the members of the second.

diferenca_conjunto([], _ , []).

diferenca_conjunto([H1|T1], L2, Res) :-
    inter_conjunto([H1|T1], L2, Inter),
    remover_elementos(Inter, [H1|T1], Res).

% remover elementos (Itens_a_remover, Lista, Lista_sem_duplicatas).
remover_elementos([], Lista , Lista ).
remover_elementos([H|T],Lista, Lista_sem_duplicatas):-
    remover_duplicadas(H,Lista,Lista_sem_head),
    !,
    remover_elementos(T, Lista_sem_head,Lista_sem_duplicatas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%% Fim dos predicados adicionados
%%%%%%%% The tests start here.
%%%%%%%% To execute the tests, use the query:   ?- run_tests.

%%%%%%%% More details about tests are available at:
%%%%%%%%    https://www.swi-prolog.org/pldoc/package/plunit.html


:- begin_tests(conjuntos).

test(uniao_conjunto, set(Ys==[[1,a,3],[1,3,a],[a,1,3],[a,3,1],[3,a,1],[3,1,a]])) :-
    uniao_conjunto([1,a], [a,3], Xs),
    mesmo_conjunto(Xs,Ys).
test(uniao_conjunto2,fail) :-
    uniao_conjunto([1,a,3,4], [1,2,3,4], [1,1,a,2,3,3,4,4]).


test(lista_para_conjunto, all(Xs=[[1,a,3,4]]) ) :-
    lista_para_conjunto([1,a,3,3,a,1,4], Xs).
test(lista_para_conjunto2,fail) :-
    lista_para_conjunto([1,a,3,3,a,1,4], [a,1,3,4]).

test(mesmo_conjunto, set(Xs=[[1,a,3],[1,3,a],[a,1,3],[a,3,1],[3,a,1],[3,1,a]])) :-
    mesmo_conjunto([1,a,3], Xs).
test(uniao_conjunto2,fail) :-
    mesmo_conjunto([1,a,3,4], [1,3,4]).

test(inter_conjunto, all(Xs==[[1,3,4]])) :-
    inter_conjunto([1,a,3,4], [1,2,3,4], Xs).
test(inter_conjunto2,fail) :-
    inter_conjunto([1,a,3,4], [1,2,3,4], [1,1,3,3,4,4]).

test(diferenca_conjunto, all(Xs==[[2]])) :-
    diferenca_conjunto([1,2,3], [3,a,1], Xs).
test(diferenca_conjunto2,fail) :-
    diferenca_conjunto([1,3,4], [1,2,3,4], [_|_]).



:- end_tests(conjuntos).







