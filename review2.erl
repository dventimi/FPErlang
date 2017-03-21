-module(review2). 
-export([get_file_contents/1,show_file_contents/1,index_file/1]).

% Used to read a file into a list of lines. 
% Example files available in: 
% gettysburg-address.txt (short) 
% dickens-christmas.txt (long)

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

% Main function to index the words of the file. 
% I prefer to return a map instead of sorted collection because I can look for 
% that word easily 
index_file(Name) -> 
    Lines = get_file_contents(Name), 
    index_file_words(Lines, 1, #{}).

% Here is done some of the hard work 
% Parameters: 
% X|Xs - Lines of the file 
% LineNumber - Current line number to be processed 
% IndexedWords - Map of the words that were indexed prior the current line. 
index_file_words([], _LineNumber, IndexedWords) -> 
    IndexedWords; 
index_file_words([X|Xs], LineNumber, IndexedWords) -> 
% First I tokenized the line string by using all this characters as separators
    Tokens = string:tokens(X, "\",;.?!-: \\"), 
% I normalized the words to lowercase 
    NormalizedTokens = lists:map(fun(Token) -> string:to_lower(Token) end, Tokens), 
% I removed all the irrelevant words which I don't want to index. 
    StopWords = get_all_stopwords(), 
    Words = lists:filter(fun(Word) -> not sets:is_element(Word, StopWords) end, NormalizedTokens),
    NewIndexedWords = process_line(Words, LineNumber, IndexedWords), 
    index_file_words(Xs, LineNumber + 1, NewIndexedWords).

% Get all stopwords to eliminate from indexing, i.e. removing irrelevant words 
get_all_stopwords() -> 
    StopwordsList = get_file_contents("stopwords.txt"), 
    sets:from_list(string:tokens(hd(StopwordsList), ", ")).

% Process the list of normalized tokens of each line 
% The way I want to represent the data is by using a map of maps: 
% { "word" => #{ <Line number> => <Number of occurrences> } } 
% because I can count the total occurrences of a word. In addition, using a map 
% for line numbers I can retrieve the lines with some word faster than an 
% using interval to represent it 
process_line([], _LineNumber, IndexedWords) -> 
    IndexedWords; 
process_line([X|Xs], LineNumber, IndexedWords) -> 
    HasWord = maps:is_key(X, IndexedWords), 
    if 
	HasWord -> 
	    Freqs = maps:get(X, IndexedWords), 
	    IsInLine = maps:is_key(LineNumber, Freqs), 
	    if 
		IsInLine -> 
		    NumOccLine = maps:get(LineNumber, Freqs), 
		    process_line(Xs, LineNumber, maps:update(X, maps:update(LineNumber, NumOccLine + 1, Freqs), IndexedWords)); 
		true -> 
		    process_line(Xs, LineNumber, maps:update(X, maps:put(LineNumber, 1, Freqs), IndexedWords)) 
	    end; 
	true -> 
	    process_line(Xs, LineNumber, maps:put(X, #{LineNumber => 1}, IndexedWords)) 
    end.
