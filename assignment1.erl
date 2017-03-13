-module(assignment1).
-export([
	 area/1,
	 bits/1,
	 dr_bits/1,
	 enclose/1,
	 perimeter/1,
	 tr_bits/1,
	 tr_bits/2
	]).
-include_lib("eunit/include/eunit.hrl").

%% NOTE: There are tests.  You should be able to run them just by
%% evaluating "assignment1:test()".  In the case of the various
%% functions on shapes, because the numerical results might be
%% floating-point numbers, the tests check that the functions compute
%% results within some tolerance, hard-coded to 1%.  I don't do this
%% for the bits/1 tests, however, since bits/1 should produce integral
%% answers.

%% Shapes

%% NOTE I: The representation I chose for a triangle comprises an
%% origin (X,Y), and 3 side lengths, S1, S2, S3.  The ORIGIN IS NOT
%% THE CENTER.  It is, in fact, the bottom-left vertex of the
%% triangle, oriented so that its longest side is on the bottom (the
%% "base"), parallel to the x-axis, and the side lengths decrease in
%% order going clockwise around the triangle, starting from the base.
%% This is important not for the perimeter/1 and area/1 definitions,
%% whose results contain no reference to the shape's location and
%% orientation.  However, it's important for the enclose/1 definition,
%% which should return a consistent representation of a rectangle
%% shape.

%% NOTE II: Likewise, the representation I chose for a rectangle
%% comprises an origin (X,Y), and its height and width, H, W.  The
%% structure of this representation is identical to that given in the
%% lecture.  The difference lies in its interpretation, in that as
%% with the triangle, THE ORIGIN IS NOT THE CENTER.  It is, in fact,
%% the bottom-left corner of the rectangle, oriented so that each of
%% its sides is parallel either to the x-axis or the y-axis.  This is
%% similar to the representation of the triangle shape, but different
%% from the representation of the circle shape.  For the circle, the
%% origin IS the center.

%% These choices were made for the following reason.  While there is
%% little ambiguity about the "center" of a circle or of a
%% "rectangle", there is considerable ambiguity about the center of a
%% "triangle" as there are several to consider: its centroid, its
%% circumcenter, its orthocenter, its incenter, etc.  The
%% relationships between these and, especially, the center of an
%% enclosing rectangle, are not trivial.  Consequently, I decided to
%% make life easy for myself by choosing favorable interpretations for
%% the representations of the triangle and rectangle shapes.

%% perimeter/1 definitions for circle, rectangle, and triangle

perimeter({circle, {_X,_Y}, R}) when R>0 ->
    math:pi()*2*R;
perimeter({rectangle, {_X,_Y}, H, W}) when H>0, W>0 ->
    2*H+2*W;
perimeter({triangle, {_X,_Y}, S1, S2, S3}) when S1>0, S2>0, S3>0 ->
    S1+S2+S3.

%% area/1 definitions for circle, rectangle, and triangle using
%% Heron's formula for the area of a triangle
%% (https://en.wikipedia.org/wiki/Heron's_formula)

area({circle, {_X,_Y}, R}) when R>0 ->
    math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) when H>0, W>0 ->
    H*W;
area({triangle, {_X,_Y}, A, B, C}) when A>0, B>0, C>0->
    P = perimeter({triangle, {_X,_Y}, A, B, C})/2,
    math:sqrt(P*(P-A)*(P-B)*(P-C)).

%% enclose/1 definitions for circle, rectangle, and triangle

enclose({circle, {X,Y}, R}) when R>0 ->
    {rectangle, {X,Y}, 2*R, 2*R};
enclose({rectangle, {X,Y}, H, W}) ->
    {rectangle, {X,Y}, H, W};
enclose({triangle, {X,Y}, S1, S2, S3}) ->
    B = lists:max([S1, S2, S3]),
    Area = area({triangle, {X,Y}, S1, S2, S3}),
    Height = 2*Area/B,
    {rectangle, {X,Y}, Height, B}.

%% Tests for the perimeter, area, and enclose functions

perimeter_test() ->
    ?assert(abs(perimeter({circle, {0,0}, 1})/2/math:pi() - 1)<0.01),
    ?assert(abs(perimeter({rectangle, {0,0}, 3, 4})/14 - 1)<0.01),
    ?assert(abs(perimeter({triangle, {0,0}, 7, 8, 9})/24 - 1)<0.01).

area_test() ->
    ?assert(abs(area({circle, {0,0}, 2})/4/math:pi() - 1)<0.01),
    ?assert(abs(area({rectangle, {0,0}, 3,4})/12 - 1)<0.01),
    ?assert(abs(area({triangle, {0,0}, 7, 8, 9})/26.8328 - 1)<0.01).

enclose_test() ->
    ?assert(enclose({circle,{0,0},1}) == {rectangle,{0,0},2,2}),
    ?assert(enclose({rectangle,{0,0},3,4}) == {rectangle,{0,0},3,4}),
    ?assert(enclose({triangle,{0,0},3,4,5}) == {rectangle,{0,0},2.4,5}).

%% Summing the bits

%% NOTE: The instructions are to define a function bits/1, but then
%% also included is the recommendation that we define both
%% direct-recursive and tail-recursive versions.  I don't know how to
%% define multiple functions with the same name and same arity in a
%% module (quite reasonably, doing so produces an error), so I created
%% the two different versions with two different names, dr_bits/1 and
%% tr_bits/1.  I then defined a bits/1 function that calls the
%% tail-recursive version.  I hope this is sufficient.

%% Direct-recursive definition supporting the bits/1 function

dr_bits(0) -> 
    0;
dr_bits(N) when N>0 -> 
    N rem 2 + dr_bits(N div 2).

%% Tail-recursive definition supporting the bits/1 function

tr_bits(N, SUM) when N>0 ->
    tr_bits(N div 2, SUM + N rem 2);
tr_bits(_N, SUM) ->
    SUM.
tr_bits(N) ->
    tr_bits(N, 0).

%% bits/1 definition that just calls the tail-recursive version

bits(N) ->
    tr_bits(N).

%% Tests for the various versions of the bits function

tr_bits_test() ->
    ?assert(tr_bits(7) == 3),
    ?assert(tr_bits(8) == 1),
    ?assert(tr_bits(109) == 5).

dr_bits_test() ->
    ?assert(dr_bits(7) == 3),
    ?assert(dr_bits(8) == 1),
    ?assert(dr_bits(109) == 5).

bits_test() ->
    ?assert(bits(7) == 3),
    ?assert(bits(8) == 1),
    ?assert(bits(109) == 5).

%% Q: Which do you think is better? Why?

%% A: There are trade-offs between the direct-recursive and the
%% tail-recursive versions.  The direct-recursive version is simpler
%% and, to me, seems like a more straightforward description of the
%% problem.  The tail-recursive version, on the other hand, in
%% principle is more efficient.  Technically, the tail-recursive
%% version probably really IS more efficient, but it's worth examining
%% the question, "By how much?"  The direct-recursive version of
%% Fibonacci made two self-calls for every step in the recursion.
%% That leads to an exponential call-graph, which is REALLY
%% inefficient.  In contrast, the direct-recursive version of bits/1
%% makes only one self-call for every step in the recursion, so its
%% call-graph complexity should be linear in the number of bits.
%% However, there is at least one other consideration, which is the
%% stack depth.  Tail-call optimization reuses the same stack frame
%% for each recursive call, but direct-recursion probably can't take
%% advantage of this, so the stack depth may grow linearly with the
%% number of bits as well.  Presumably, for very large numbers with
%% many bits, the direct-recursive version may cause a "stack
%% overflow" type error.  This is a common problem with deep recursion
%% in other languages.  I don't know if it is with Erlang, but I'm
%% guessing that it is.  Bear in mind, this should only occur for very
%% large numbers, with many bits.  However, given that Erlang supports
%% arbitrary precision Bignums, it seems like it should be a genuine
%% worry.  Considering all of these factors, it is my opinion that the
%% tail-recursive version of the bits/1 function is in some sense
%% "better."

%%  LocalWords:  dr eunit sqrt
