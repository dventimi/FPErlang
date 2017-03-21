-module(rockpaperscissors).
-export([
	 beat/1,
	 lose/1,
	 result/2,
	 tournament/2
	]).
-include_lib("eunit/include/eunit.hrl").

beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.

lose(rock) ->
    scissors;
lose(paper) ->
    rock;
lose(scissors) ->
    paper.

result(First,Second) when First==Second ->
    0;
result(First,Second) ->
    case beat(Second) of
	First ->
	    1;
	_ -> -1
    end.

tournament(Left,Right) ->
    lists:foldr(fun(X,Y) ->
			X+Y 
		end, 
		0,
		lists:map(fun({First,Second}) -> 
				  result(First,Second) 
			  end, 
			  lists:zip(Left,Right))).

tournament_test() ->
    ?assert(tournament([rock,rock,paper,paper],[rock,paper,scissors,rock])==-1).
