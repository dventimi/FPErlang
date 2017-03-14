-module(practice).
-export([
	 double/1,
	 dr_max/1,
	 dr_product/1,
	 evens/1,
	 len/1,
	 max/1,
	 median/1,
	 nth/2,
	 product/1,
	 sort/1,
	 tr_max/2,
	 tr_product/2
	]).

product(X) ->
    %% dr_product(X).
    tr_product(X, 1).

dr_product([]) ->
    1;
dr_product([X|Xs]) ->
    X*dr_product(Xs).

tr_product([], P) ->
    P;
tr_product([X|Xs], P) ->
    tr_product(Xs, X*P).


max(X) ->
    %% dr_max(X).
    tr_max(X,0).

dr_max([X]) ->
    X;
dr_max([X|Xs]) ->
    max(X,dr_max(Xs)).

tr_max([], M) ->
    M;
tr_max([X|Xs], M) ->
    tr_max(Xs, max(X,M)).


double([]) ->
    [];
double([X|Xs]) when X rem 1 == 0 ->
    [2*X|double(Xs)].

evens([]) ->
    [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X|evens(Xs)];
evens([_X|Xs]) ->
    evens(Xs).

len([]) ->
    0;
len([_X|Xs]) ->
    1+length(Xs).

nth(0,[X|_Xs]) ->
    X;
nth(N,[_X|Xs]) ->
    nth(N-1,Xs).

sort([]) ->
    [];
sort([X|[Y|Xs]]) when X>Y ->
    sort([X,Y|Xs]);
sort([X|[Y|Xs]]) ->
    sort([Y,X|Xs]).

median(X) ->
    Size = len(X),
    case Size rem 2 of
	1 ->
	    nth(Size div 2, X);
	_Else ->
	    (nth(Size div 2 - 1, X) + nth(Size div 2, X))/2
    end.
    
