-module(hof).
-export([
	 add/1,
	 times/1,
	 compose/1,
	 compose/2,
	 id/1,
	 iterate/1,
	 once/1,
	 twice/1
	]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

compose(Fs) ->
    lists:foldr(fun compose/2,
		fun (X) -> X end,
		Fs).

once(F) ->
    fun(X) ->
	    F(X) end.

twice(F) ->
    compose([F,F]).

id(X) ->
    X.

iterate(0) ->
    fun (_F) ->
	    fun id/1 end;
iterate(N) ->
    fun (F) ->
	    compose(F, (iterate(N-1))(F)) end.

%% How would you define it using lists:map, lists:foldr, and compose?
