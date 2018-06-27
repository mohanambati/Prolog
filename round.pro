%----------------------------------------------------------------------------------------------------------------------------%
%
% Program Name: hw1.pro
% written by: Mohan Sai Ambati (10072130)
% Description:
%		This program takes the From and To location of a trip, diameters of the roundabouts and paths with the in and out angles
% and finds all possible routes form start to end points and caliculates the distance of that particular route.
%-----------------------------------------------------------------------------------------------------------------------------%

%%%%%%%%%%%%%%% INPUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-----------Given Input ---------------------------%
trip(1,7).
diameters([1000,1000,1000,1000,400,1000,1000]).
roads([[1,2,500,225,45], [1,3,500,315,135],
[2,4,500,315,135], [3,4,500,225,45],
[4,5,500,225,45], [4,6,500,315,135],
[5,7,500,315,135], [6,7,500,225,45]] ).


%%%%%%%%%%%%%%% go Module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-----------go function---------------------------%
go:-
trip(From,To), 
find(From,To).

%%%%%%%%%%%%%%% Route Finder Module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- This function caliculates all possible routes from the given input data-------%

find(From,To):-
getroute( From, To, Route), % From , To are inputs and Route is the output.
distance(Route), % this function calicualtes the distance for the route found above.
print( ' Route = ' ), print( Route ), nl, fail.


getroute( From, To, Route) :- 
	findroutes( [From], To, RevRoute),
	reverse( RevRoute, Route ).

findroutes( [To|Before], To, [To|Before]).

findroutes( [At|Before], To, FinalRoute) :-
	new(At,New),
	\+ member( New, [At|Before] ),
	findroutes( [New,At|Before], To, FinalRoute).

%------ This function finds the new possibility to each end point from the present location -----%

new(At,New):-
roads(L),
newroute(At,New,L).


newroute(_,_,[]).

newroute(At,New,[[H|T]|B]):-
newroute(At,New,B).

newroute(At,New,[[At,H|T1]|B]):-
New is H.

%------ This function gives the first element in a list -----%
first([],_).

first([H|_],Item):-
Item is H.


%------ This function gives the second element in a list -----%

second([],_).
second([H1,H2|_],Sec):-
Sec is H2.


%%%%%%%%%%%%%%% Distance Module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------ This function caliculate ths distance for a give route ---%

distance([]). 

distance(H):-
diameters(D),
roads(R),
caldis(D,R,H,Distance,0,Angle).


%------ This function caliculates the distance --------------------%


caldis(D,Rest,[],Distance,N,Angle).


caldis(D,[],[At|T],Distance,N,Angle):-
print('Distance='), print(Distance).

caldis(D,[[At,To,Dis,Presentout,Nextin]|Rest],[H|T],Distance,N,Angle):-
first(T,H2),
((At == H, To == H2) ->
		 ((N == 0 ->
		Total is Dis,
		Inangle is Nextin,
		caldis(D,[[At,To,Dis,PresentOut,Nextin]|Rest],T,Total,1,Inangle);

		my_nth(H,D,Dia),
		Outangle is Presentout,
		Changeangle is max(Angle,Outangle) - min(Angle,Outangle),
		Total is Distance + Dis + (Dia * 3.14159 * (Changeangle/360)),
		Inangle is Nextin,
		caldis(D,[[At,To,Dis,PresentOut,Nextin]|Rest],T,Total,1,Inangle)
		));
caldis(D,Rest,[H|T],Distance,N,Angle)
		).

%------ This function returns the element in Nth position in a list --------------------%

my_nth( N, List, Item  ) :-
	nth_helper( N, List, 1, Item ).

nth_helper( Desire, [Item|_], Desire, Item ).

nth_helper( Desire, [_|More], Now, Item ) :-
	NewNow is Now + 1,
	nth_helper( Desire, More, NewNow, Item ).

%---------------------------------------------------------End of Program ------------------------------------------------------%





