-module(practice).
-export([
	 cat/1,
	 concat/1,
	 double/1,
	 doubleAll/1,
	 dr_max/1,
	 dr_product/1,
	 evens/1,
	 insert/2,
	 insertionsort/1,
	 join/2,
	 len/1,
	 max/1,
	 median/1,
	 member/2,
	 mergesort/1,
	 mode/1,
	 nub/1,
	 palindrome/1,
	 partition/1,
	 partition/2,
	 perms/1,
	 product/1,
	 pshunt/4,
	 quicksort/1,
	 reverse/1,
	 shunt/2,
	 my_nth/2,
	 stripchars/2,
	 take/2,
	 zip/2,
	 zip/3,
	 zip_with/3
	]).
-include_lib("eunit/include/eunit.hrl").

%% product

%% product(X) ->
%%     %% dr_product(X).
%%     tr_product(X, 1).

%% direct-recursive product

dr_product([]) ->
    1;
dr_product([X|Xs]) ->
    X*dr_product(Xs).

%% tail-recursive product

tr_product([], P) ->
    P;
tr_product([X|Xs], P) ->
    tr_product(Xs, X*P).

%% max

max(X) ->
    %% dr_max(X).
    tr_max(X,0).

%% direct-recursive max

dr_max([X]) ->
    X;
dr_max([X|Xs]) ->
    max(X,dr_max(Xs)).

%% tail-recursive max

tr_max([], M) ->
    M;
tr_max([X|Xs], M) ->
    tr_max(Xs, max(X,M)).

%% double

double([]) ->
    [];
double([X|Xs]) when X rem 1 == 0 ->
    [2*X|double(Xs)].

%% evens

%% evens([]) ->
%%     [];
%% evens([X|Xs]) when X rem 2 == 0 ->
%%     [X|evens(Xs)];
%% evens([_X|Xs]) ->
%%     evens(Xs).

%% median

len([]) ->
    0;
len([_X|Xs]) ->
    1+length(Xs).

nth(0,[X|_Xs]) ->
    X;
nth(N,[_X|Xs]) ->
    nth(N-1,Xs).

median([X|Xs]) ->
    L = lists:sort([X|Xs]),
    Size = len(L),
    case Size rem 2 of
	1 ->
	    nth(Size div 2, L);
	_Else ->
	    (nth(Size div 2 - 1, L) + nth(Size div 2, L))/2
    end.

%% mode

occurrences(_N,[]) ->
    0;
occurrences(N,[X|Xs]) when N==X ->
    1 + occurrences(N, Xs);
occurrences(N,[_X|Xs]) ->
    0 + occurrences(N, Xs).

occurrences([]) ->
    [];
occurrences([X|Xs]) ->
    [{X,occurrences(X,[X|Xs])}|occurrences(Xs)].

mode([X|Xs]) ->
    [{A,_NA}|_Os] = lists:sort(fun({_A,NA},{_B,NB})->
				       NA>NB end, occurrences([X|Xs])),
    A.

%% take

take(0,[_X|_Xs]) ->
    [];
take(_,[]) ->
    [];
take(N,[X|Xs]) when N>0 ->
    [X|take(N-1,Xs)].

take_test() ->
    ?assert(take(0, "hello")==[]),
    ?assert(take(4, "hello")=="hell"),
    ?assert(take(5, "hello")=="hello"),
    ?assert(take(9, "hello")=="hello").

%% nub

dedup([]) ->
    [];
dedup([X|Xs]) ->
    dedup([X|Xs],[]).
dedup([],L)->
    L;
dedup([X|Xs],L) ->
    dedup(Xs, lists:keystore(X, 1, L, {X})).
 
untuple([]) ->
    [];
untuple([{A}|Xs]) ->
    [A|untuple(Xs)].

nub([]) ->
    [];
nub([X|Xs]) ->
    untuple(dedup([X|Xs])).

%% palindrome

stripchars(Chars, List1) ->
    lists:filter(fun(X) ->
			 not lists:member(X, Chars) end, List1).

palindrome_test() ->
    ?assert(practice:palindrome("Madam I\'m Adam")).

palindrome([]) ->
    true;
palindrome(Xs) ->
    S = string:to_upper(stripchars("., \'\"\t\n", Xs)),
    S == lists:reverse(S).

%% join

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

reverse(Xs) ->
    shunt(Xs,[]).

cat(Xs) ->
    shunt(shunt(Xs,[]),[]).

join(Xs,Ys) ->
    shunt(reverse(Xs),Ys).

%% concat

concat([]) ->
    [];
concat([X|Xs]) ->
    join(X, concat(Xs)).

%% member

member(_X,[]) ->
    false;
member(X,[X|_Xs]) ->
    true;
member(Y,[_X|Xs]) ->
    member(Y,Xs).

%% sorting

mergesort([]) ->
    [];
mergesort([X]) ->
    [X];
mergesort([X|Xs]) ->
    {Front,Back} = lists:split(len([X|Xs]) div 2, [X|Xs]),
    lists:merge(mergesort(Front), mergesort(Back)).

%% quicksort([]) ->
%%     [];
%% quicksort([X]) ->
%%     [X].
%% quicksort([X|Xs],P) when X<P ->
%%     quicksort(Xs, 

pshunt([],Ys,Zs,_P) ->
    {Ys,Zs};
pshunt([X|Xs],Ys,Zs,P) when X<P ->
    pshunt(Xs,[X|Ys],Zs,P);
pshunt([X|Xs],Ys,Zs,P) ->
    pshunt(Xs,Ys,[X|Zs],P).

partition(Xs,P) ->    
    pshunt(Xs,[],[],P).
    
partition([X|Xs]) ->
    partition([X|Xs],X).

quicksort([]) ->
    [];
quicksort([X]) ->
    [X];
quicksort([X|Xs]) ->
    {Left,Right} = partition([X|Xs]),
    quicksort(Left)++quicksort(Right).

insertionsort([]) ->
    [];
insertionsort([X]) ->
    [X];
insertionsort([X|Xs]) ->
    insert(X, insertionsort(Xs)).

insert(Y, []) ->
    [Y];
insert(Y, [X|Xs]) when Y<X ->
    [Y,X|Xs];
insert(Y, [X|Xs]) ->
    [X|insert(Y,Xs)].

%% permutations

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%% Using higher-order functions

%% Define the functions doubleAll, evens, and product using the
%% higher-order functions lists:map, lists:filter and lists:foldr.

doubleAll(List) ->
    lists:map(fun(X) ->
		      2*X end, List).

evens(List) ->
    lists:filter(fun(X) ->
			 X rem 2 == 0 end, List).

product(List) ->
    lists:foldr(fun(X,Y) ->
			X*Y end, 1, List).
			   
%% Zipping

%% a) Define a function zip/2 that “zips together” pairs of elements
%% from two lists like this:

%% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]

%% where you can see that the elements from the longer list are lost.

%% zip([X|Xs], [Y|Ys]) ->
%%     [{X,Y}|zip(Xs,Ys)];
%% zip([],[_Y|_Ys]) ->
%%     [];
%% zip([_X|_Xs],[]) ->
%%     [];
%% zip([],[]) ->
%%     [].

zip([X|Xs], [Y|Ys], [Z|Zs]) ->
    [{X,Y,Z}|zip(Xs,Ys,Zs)];
zip(_,_,_) ->
    [].

%% b) Define a function zip_with/3 that “zips together” pairs of
%% elements from two lists using the function in the first argument,
%% like this:

%% zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]

zip_with(F,[X|Xs],[Y|Ys]) ->
    [F(X,Y)|zip_with(F,Xs,Ys)];
zip_with(_,_,_) ->
    [].

%% c) Re-define the function zip_with/3 using zip and lists:map.

%% zip_with(F,List1,List2) ->
%%     lists:map(fun({X,Y}) -> F(X,Y) end, zip(List1,List2)).

%% d) Re-define zip/2 using zip_with/3.

zip(List1,List2) ->
    zip_with(fun(X,Y) ->
		     {X,Y} end, List1, List2).

my_nth(0,[X|_]) -> X;
my_nth(N,[_|Xs]) -> nth(N-1,Xs).

%% my_nth(N,[_|Xs]) -> nth(N-1,Xs);
%% my_nth(0,[X|_]) -> X.

