%% Vilpesh’s assignment
%% VM
%% Submitted by
%% Vilpesh Mistry
%% Shapes :- 
%% I could not understand what all functions need to be constructed. I
%% only understood to create area ,perimeter. How does area gets
%% called when perimeter is called. Where is enclose function getting
%% called and for which specific shape. So i did not do the assignment
%% on Shape.

-module(review1).
-export([bits/1]).

%% Answer to Summing the bits below:- 
bits(0) -> 0; 
bits(1) -> 1; 
bits(N) -> bits(N, 0). 
bits(N,R) when ((N > 0) and (N rem 2 == 1)) -> bits(N div 2, R+1); 
bits(N,R) when ((N > 0) and (N rem 2 == 0)) -> bits(N div 2, R+0); 
bits(1,R) -> R+1; 
bits(0,R) -> R.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. The solution appears to be correct.  I tried it with bits(7),
%% bits(8), and bits(109).

%% 
