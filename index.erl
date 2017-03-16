-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([
	 assignvalue/2,
	 encode/1,
	 enumerate/1,
	 flattenindex/1,
	 index/1,
	 indexline/1,
	 indexlines/1,
	 indexwords/1
	]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
show_file_contents([]) ->
    ok.    

%% Main functions

enumerate(List) -> 
    lists:zip(List, lists:seq(1, length(List))).

assignvalue(List,Value) ->
    lists:map(
      fun(Y) ->
	      {Y, Value} 
      end, List).

words(List) ->
    string:tokens(List, " .,-\\][").

indexline({List, LineNumber}) ->
    assignvalue(words(List), LineNumber).

indexlines(List) ->
    lists:flatmap(
      fun(X) ->
	      indexline(X) 
      end, List).

indexwords(FileName) ->
    lists:sort(
      fun({Word1,_},{Word2,_}) ->
	      Word1=<Word2 
      end, indexlines(
	     lists:filter(
	       fun({Word,_}) -> 
		       length(Word)>0 
	       end, enumerate(
		      get_file_contents(FileName))))).

flattenindex(Index) ->
    dict:to_list(
      lists:foldl(
	fun({Word,LineNumber},OldDict)->
		dict:append(Word, LineNumber, OldDict) 
	end, dict:new(), Index)).

encode(List) ->
    lists:reverse(
      lists:foldl(
	fun(Current,[{BaseValue,Previous}|Acc]) when Current==Previous+1 ->
		[{BaseValue,Current}|Acc];
	   (Current,Acc) ->
		[{Current,Current}|Acc]
	end, [], List)).

index(FileName) ->
    lists:map(
      fun({Word,Index}) ->
	      {Word,encode(Index)}
      end, flattenindex(indexwords(FileName))).

