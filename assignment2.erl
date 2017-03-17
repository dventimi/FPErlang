%% FutureLearn:  Functional Programming in Erlang
%% 
%% Assignment 2
%% 
%% Objective: The aim of this exercise is to index a text file, by
%% line number.
%% 
%% Strategy: The approach is to build up from smaller and more general
%% functions to larger and more specific functions.  Roughly, the
%% steps are these.
%% 
%% 1. Get lines of text from a text file (this is handled already by
%% the provided get_file_contents function).
%% 
%% 2. "Enumerate" the line numbers by associating each line of text
%% with the line number on which it appears in the file (starting at 1).
%% 
%% 3. For each line, split it into words and "scatter" the line number
%% across all of those words, generating a list of {Word,LineNumber}
%% tuples.  All the lines of the file therefore will produce a List of
%% Lists.
%% 
%% 4. Join that List of Lists (of Tuples) into one giant List (of
%% Tuples).
%% 
%% 5. Sort that giant TupleList lexicographically according to the
%% first element of each Tuple, which of course is the word.  This
%% brings identical words together so that they're adjacent in the
%% list.
%% 
%% 6. "Flatten" that giant TupleList (which may contain duplicate
%% words) to a smaller TupleList by "combining" duplicates.  The
%% combining operation for Tuples whose Word elements are the same is
%% to merge their corresponding LineNumber elements into a list of
%% LineNumbers.  For example:
%% 
%%   [{"cat",5}, {"cat",6}, {"cat",7}, {"cat",10}] -> [{"cat",[5,6,7,10]}]
%% 
%% 7. "Encode" those merged Tuples using a variation on Run-Length
%% Encoding in order to generate inclusive ranges of line-numbers.
%% For example:
%% 
%%   [{"cat",[5,6,7,10]}] -> [{"cat",[{5,7},{10,10}]}]
%% 
%% 8. Along the way, add in some additional features:
%% 
%%    a. filter out "short" words
%%    b. filter out certain "common" words
%%    c. normalize to lower-case so that "God" and "god" are treated as just "god"
%%    d. do some light unit testing
%% 

-module(assignment2).
-export([
	 assign_value/2,
	 encode/1,
	 enumerate/1,
	 flatten_index/1,
	 index/1,
	 index_enumerated_lines/1,
	 index_words/1,
	 common_word/1,
	 short_word/1,
	 word_scatter/1,
	 words/1
	]).
-include_lib("eunit/include/eunit.hrl").

%% Enumerate the elements of List sequentially and generate a
%% TupleList, in which each Tuple maps an element E to its ordinal
%% position within the list, beginning with 1.
enumerate(List) -> 
    lists:zip(List, lists:seq(1, length(List))).

%% Test enumerate on a List of lines obtained from a text file.
%% Verify that the first line is mapped to 1 and the second line is
%% mapped to 2.  NOTE: we use the take/2 function defined in the
%% practice module just to make life a little easy on ourselves, since
%% the test files actually contain a lot of data.
enumerate_test() ->
    ?assert(
       practice:take(2, 
		     enumerate(
		       index:get_file_contents(
			 "gettysburg-address.txt")))==
	   [{"Four score and seven years ago our fathers brought",1},
	    {"forth on this continent, a new nation, conceived in Liberty,", 2}]).

%% Split String into a List of words, identified by common word
%% boundary characters (spaces, punctuation, etc.).
words(String) ->
    string:tokens(String, " .,-\\][").

%% Test words on a handful of representative String values.
words_test() ->
    ?assert(words("")==[]),
    ?assert(words("The quick brown fox jumped over the lazy dogs.")==
		["The","quick","brown","fox","jumped","over","the","lazy", "dogs"]).

%% Given a List and a Value, generate a TupleList in which each List
%% element is associated with a copy of the Value.
assign_value(List,Value) ->
    lists:map(
      fun(Y) ->
	      {Y, Value} 
      end, List).

%% Test assign_value on a list of words taken from a common phrase and
%% verify that a copy of the correct value (1 in this case) is
%% assigned to each word in a new TupleList.
assign_value_test() ->
    ?assert(assign_value(["The",
			 "quick",
			 "brown",
			 "fox",
			 "jumped",
			 "over",
			 "the",
			 "lazy", 
			 "dogs"], 1)==
		[{"The",1}, 
		 {"quick",1}, 
		 {"brown",1}, 
		 {"fox",1}, 
		 {"jumped",1}, 
		 {"over",1}, 
		 {"the",1}, 
		 {"lazy",1}, 
		 {"dogs",1}]).

%% Given a String that represents 0 or more words and a Value (perhaps
%% a line number), split the String into a List of words and scatter
%% the Value across all of the words in the List.
word_scatter({String, Value}) ->
    assign_value(words(String), Value).

%% Given a TupleList in which each Tuple associates a String with a
%% Value (e.g., a line of text with its corresponding line number),
%% index the String by splitting it into Words and scattering the
%% Value across the Words.  The result is another TupleList in which
%% each Tuple associates a Word with a Value.  If, as in the example,
%% the words are from enumerated lines of text this has the effect of
%% generating a list of {word, line-number} pairs.  Naturally, words
%% may be duplicated, since the same word may appear in any number of
%% lines.
index_enumerated_lines(List) ->
    lists:flatmap(
      fun(X) ->
	      word_scatter(X) 
      end, List).

%% Test index_enumerated_lines on a List of the first 3 lines of text
%% taken from The Gettysburg Address, and validate that it generates
%% the appropriate indext word list, in which each word is associated
%% with a copy of the line-number in which it appears (words may
%% appear multiple times on different line numbers.)
index_enumerated_lines_test() ->
    ?assert(
       practice:take(
	 27, 
	 index_enumerated_lines(
	   [{"Four score and seven years ago our fathers brought", 1},
	    {"forth on this continent, a new nation, conceived in Liberty,", 2},
	    {"and dedicated to the proposition that all men are created equal.", 3}]))==
	   [{"Four",1},
	    {"score",1},
	    {"and",1},
	    {"seven",1},
	    {"years",1},
	    {"ago",1},
	    {"our",1},
	    {"fathers",1},
	    {"brought",1},
	    {"forth",2},
	    {"on",2},
	    {"this",2},
	    {"continent",2},
	    {"a",2},
	    {"new",2},
	    {"nation",2},
	    {"conceived",2},
	    {"in",2},
	    {"Liberty",2},
	    {"and",3},
	    {"dedicated",3},
	    {"to",3},
	    {"the",3},
	    {"proposition",3},
	    {"that",3},
	    {"all",3},
	    {"men",3}]).

%% Index the words in a file named by Filename.  That is, generate a
%% TupleList in which each Tuple associates a word from the file with
%% the line-number on which it appears.  The list is sorted
%% lexicographically by the word.  Moreover, since words are common
%% and any given word may appear in many places throughout a text,
%% even on the same line, words and even {word,line-number} pairs may
%% be duplicated.  This function makes no attempt to cope with
%% duplicates.  Note that this is a higher-level function.  I.e., it
%% brings together functionality from a variety of functions.  That
%% means its behavior is more specific and less general.  There's a
%% trade-off between those two, with this favoring specificity over
%% generality.  Consequently, it's composability with other functions
%% is diminished.
index_words(FileName) ->
    lists:sort(
      fun({Word1,_},{Word2,_}) ->
	      Word1=<Word2 
      end, 
      lists:map(fun({Word,LineNumber}) ->
			{string:to_lower(Word), LineNumber}  %Normalize to lowercase.
		end, index_enumerated_lines(
		       lists:filter(
			 fun({Line,_}) -> 			     
				 length(Line)>0		     %Kill empty lines.
			 end, enumerate(
				index:get_file_contents(FileName)))))).

%% Test index_words on the file "gettysburg-address.txt" and validate
%% that it generates the correct {word,line-number} index list.
index_words_test()->
    ?assert(practice:take(27, index_words("gettysburg-address.txt"))==
		[{"a",2},
		 {"a",5},
		 {"a",7},
		 {"a",8},
		 {"a",8},
		 {"a",13},
		 {"a",26},
		 {"above",16},
		 {"add",16},
		 {"advanced",20},
		 {"ago",1},
		 {"all",3},
		 {"altogether",10},
		 {"and",1},
		 {"and",3},
		 {"and",6},
		 {"and",10},
		 {"and",15},
		 {"and",27},
		 {"any",6},
		 {"are",3},
		 {"are",5},
		 {"are",7},
		 {"as",8},
		 {"battle",7},
		 {"be",19},
		 {"be",21}]).

%% Flatten a word index list Index, which may contain duplicates of
%% the same word on different lines or even on the same line.  The
%% input Index is a lexicographically-sorted TupleList in which each
%% Tuple associates a single word with a single line number.  The
%% flattened output is another TupleList in which each Tuple
%% associates a word with a list of line numbers.  Note that while
%% words may be duplicated in the input TupleList Index, words will
%% not be duplicated in the output TupleList.
flatten_index(Index) ->
    dict:to_list(
      lists:foldl(
	fun({Word,LineNumber},OldDict)->
		dict:append(Word, LineNumber, OldDict) 
	end, dict:new(), Index)).

%% Test flatten_index on an input word index list, in which words may
%% be duplicated, and validate that it generates the correct flattened
%% output index list.
flatten_index_test() ->
    ?assert(flatten_index([{"But",13},
			   {"Four",1},
			   {"God",26},
			   {"It",10},
			   {"It",18},
			   {"It",21},
			   {"Liberty",2},
			   {"Now",5},
			   {"The",15},
			   {"The",17},
			   {"We",7},
			   {"We",8},
			   {"a",2},
			   {"a",5},
			   {"a",7},
			   {"a",8},
			   {"a",8},
			   {"a",13},
			   {"a",26},
			   {"above",16},
			   {"add",16},
			   {"advanced",20},
			   {"ago",1},
			   {"all",3},
			   {"altogether",10},
			   {"and",1},
			   {"and",3}])==
		[{"It",[10,18,21]},
		 {"God",[26]},
		 {"ago",[1]},
		 {"Now",[5]},
		 {"all",[3]},
		 {"add",[16]},
		 {"advanced",[20]},
		 {"and",[1,3]},
		 {"But","\r"},
		 {"above",[16]},
		 {"altogether","\n"},
		 {"We",[7,8]},
		 {"Four",[1]},
		 {"a",[2,5,7,8,8,13,26]},
		 {"The",[15,17]},
		 {"Liberty",[2]}]).

%% Encode a list of Integers--sorted in ascending order, and with or
%% withot duplicates--using a variation of Run-Length Encoding (RLE).
%% See: https://en.wikipedia.org/wiki/Run-length_encoding.  Instead of
%% generating a run of identical numbers (which would comprise the
%% number and the length of the run), partition the list into
%% intervals that encompass runs wherein adjacent integers differ by
%% either 0 or 1.  The output is a TupleList in which each Tuple is an
%% ordered-pair corresponding to a closed interval.
encode(List) ->
    lists:reverse(
      lists:foldl(
	fun(Current,[{BaseValue,Previous}|Acc]) when (Current-Previous)=<1 ->
		[{BaseValue,Current}|Acc];
	   (Current,Acc) ->
		[{Current,Current}|Acc]
	end, [], List)).

%% Test the encode function in a handful of cases.
encode_test() ->
    ?assert(encode([1,1,2,3,4,5,10,11,12])==[{1,5},{10,12}]),
    ?assert(encode([1,2,3,4,5,10,11,12])==[{1,5},{10,12}]),
    ?assert(encode([1,2,3,4,5,6,7,8,9,10,11,12])==[{1,12}]).



%% Filter out words from List when their length is less than 3.
short_word(Word) ->
    length(Word)<3.


%% Test kill_short_words function on a common phrase.
short_word_test() ->
    ?assert(
       lists:filter(fun(Word) -> not short_word(Word) end,
	 words(
	   "Now is the time for all good men to come to the aid of their country."))==
	   ["Now",
	    "the",
	    "time",
	    "for",
	    "all",
	    "good",
	    "men",
	    "come",
	    "the",
	    "aid",
	    "their",
	    "country"]).

%% Identify common words
common_word(Word) ->
    lists:member(Word, 
		 words("is to of the a an for from in on but and")).

%% Test the kill_common_words function on a common phrase.
common_words_test() ->
    ?assert(
       lists:filter(fun(Word) -> not common_word(Word) end,
	 words(
	   "Now is the time for all good men to come to the aid of their country."))==
	   ["Now",
	    "time",
	    "all",
	    "good",
	    "men",
	    "come",
	    "aid",
	    "their",
	    "country"]).

%% Index a text file, by line number. This the main function.  The
%% output of the main function should be a list of entries consisting
%% of a word and a list of the ranges of lines on which it occurs.
%% 
%% For example, the entry
%% 
%% { "foo" , [{3,5},{7,7},{11,13}] }
%% 
%% means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13
%% in the file.
index(FileName) ->
    lists:filter(fun({Word,_}) ->
			 not common_word(Word) and not short_word(Word)
		 end,
		 lists:map(
		   fun({Word,Index}) ->
			   {Word,encode(Index)}
		   end, flatten_index(index_words(FileName)))).

%% Test the main function index on the text of The Gettysburg Address.
index_test() ->
    ?assert(practice:take(24, index("gettysburg-address.txt"))==
		[{"say",[{17,17}]},
		 {"larger",[{13,13}]},
		 {"fought",[{20,20}]},
		 {"war",[{5,5},{7,7}]},
		 {"they",[{18,18},{20,20},{23,23}]},
		 {"measure",[{24,24}]},
		 {"god",[{26,26}]},
		 {"hallow",[{14,14}]},
		 {"ground",[{14,14}]},
		 {"forget",[{18,18}]},
		 {"ago",[{1,1}]},
		 {"years",[{1,1}]},
		 {"last",[{24,24}]},
		 {"battle",[{7,7}]},
		 {"that",[{3,3},{6,11},{22,27}]},
		 {"did",[{18,18}]},
		 {"all",[{3,3}]},
		 {"vain",[{25,25}]},
		 {"under",[{26,26}]},
		 {"proposition",[{3,3}]},
		 {"advanced",[{20,20}]},
		 {"lives",[{9,9}]},
		 {"brought",[{1,1}]},
		 {"world",[{17,17}]}]).

%% DONE: Indexing a file.

%% DONE: Removing all short words (e.g. words of length less than 3)
%% or all common words (you‘ll have to think about how to define
%% these).

%% DONE: Sorting the output so that the words occur in lexicographic
%% order.

%% DONE: Normalising the words so that capitalised ("Foo") and non
%% capitalised versions ("foo") of a word are identified.

%% TODO: Normalising so that common endings, plurals etc. identified.

%% TODO: (Harder) Thinking how you could make the data representation
%% more efficient than the one you first chose. This might be
%% efficient for lookup only, or for both creation and lookup.

%% TODO: Can you think of other ways that you might extend your
%% solution?
