-module(practice).
-export([
	 double/1,
	 dr_max/1,
	 dr_product/1,
	 evens/1,
	 len/1,
	 max/1,
	 median/1,
	 mode/1,
	 nth/2,
	 occurrences/1,
	 occurrences/2,
	 product/1,
	 take/2,
	 tr_max/2,
	 tr_product/2
	]).
-include_lib("eunit/include/eunit.hrl").

%% product

product(X) ->
    %% dr_product(X).
    tr_product(X, 1).

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

evens([]) ->
    [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X|evens(Xs)];
evens([_X|Xs]) ->
    evens(Xs).

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
take(N,[X|Xs]) ->
    [X|take(N-1,Xs)].

take_test() ->
    ?assert(take(0, "hello")==[]),
    ?assert(take(4, "hello")=="hell"),
    ?assert(take(5, "hello")=="hello"),
    ?assert(take(9, "hello")=="hello").
