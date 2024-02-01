% Define a predicate for finding a solution
solution(Solution) :-
    initial(Initial),
    final(Final),
    path(Initial, Solution, Final).

% Define the initial and final states
initial(miscan(3, 3, 0, 0, left)).
final(miscan(0, 0, 3, 3, right)).

% Define the main path-finding predicate
path(Initial, Path, Final) :-
    path(Initial, Path, Final, []).

% Base case: the path is valid if the current state is the final state
path(Initial, [Crossing], Final, _) :-
    valid(Crossing, Initial, Final).

% Recursive case: find a valid path by exploring different crossings
%The notation [crossing | rest] 
%constructs a list in which the first element is First and the rest is Tail.
path(Initial, [Crossing | Rest], Final, Visited) :-
    valid(Crossing, Initial, Intermediate),
    not_in(Intermediate, Visited),
    path(Intermediate, Rest, Final, [Initial | Visited]).

% Define the validity conditions for boat crossings

% Define rules for one person to travel on the boat
valid(boat(1, 0, left), % solo se mueve una persona en el bote
%(misionero,canibal, a donde va)
    miscan(MI, CI, MD, CD, right), % estimo si es que los canibales no se pueden comer a los misioneros
    miscan(NI, CI, ND, CD, left)) :- % estimo si es que los canibales no se pueden comer a los misioneros
    % Conditions for valid crossing
    ND is MD - 1, %misioneros = canibales  en el bote y se quita de donde esataba
    NI is MI + 1, %canibales = misioneros  en el bote y se quita de donde esataba
    ND >= 0, %inicio 
    NI =< 3, %fin
    (NI = 0 ; NI >= CI), % si se cumple se mueve
    (ND = 0 ; ND >= CD). % si se cumple se puede mover

valid(boat(1, 0, right), %(misionero,canibal, a donde va)
    miscan(MI, CI, MD, CD, left), % estimo si es que los canibales no se pueden comer a los misioneros
    miscan(NI, CI, ND, CD, right)) :-% estimo si es que los canibales no se pueden comer a los misioneros
    % Conditions for valid crossing
    NI is MI - 1,%misioneros = canibales  en el bote y se quita de donde esataba
    ND is MD + 1, %canibales = misioneros en el bote y se quita de donde esataba
    NI >= 0,%inicio
    ND =< 3,%fin
    (NI = 0; NI >= CI),% si se cumple se mueve
    (ND = 0; ND >= CD). % si se cumple se puede mover

valid(boat(0, 1, left),
    miscan(MI, CI, MD, CD, right),
    miscan(MI, DI, MD, DD, left)) :-
    % Conditions for valid crossing
    DD is CD - 1,
    DI is CI + 1,
    DD >= 0,
    DI =< 3,
    (MI = 0; MI >= DI),
    (MD = 0; MD >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
valid(boat(0, 1, right),
    miscan(MI, CI, MD, CD, left),
    miscan(MI, DI, MD, DD, right)) :-
    % Conditions for valid crossing
    DI is CI - 1,
    DD is CD + 1,
    DI >= 0,
    DD =< 3,
    (MI = 0; MI >= DI),
    (MD = 0; MD >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros  en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover

% Define rules for two people to travel (of the same type)
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
valid(boat(2, 0, left),
    miscan(MI, CI, MD, CD, right),
    miscan(NI, CI, ND, CD, left)) :-
    % Conditions for valid crossing
    ND is MD - 2,
    NI is MI + 2,
    ND >= 0,
    NI =< 3,
    (NI = 0; NI >= CI),
    (ND = 0; ND >= CD).

valid(boat(2, 0, right),
    miscan(MI, CI, MD, CD, left),
    miscan(NI, CI, ND, CD, right)) :-
    % Conditions for valid crossing
    NI is MI - 2,
    ND is MD + 2,
    NI >= 0,
    ND =< 3,
    (NI = 0; NI >= CI),
    (ND = 0; ND >= CD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
valid(boat(0, 2, left),
    miscan(MI, CI, MD, CD, right),
    miscan(MI, DI, MD, DD, left)) :-
    % Conditions for valid crossing
    DD is CD - 2,
    DI is CI + 2,
    DD >= 0,
    DI =< 3,
    (MI = 0; MI >= DI),
    (MD = 0; MD >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
valid(boat(0, 2, right),
    miscan(MI, CI, MD, CD, left),
    miscan(MI, DI, MD, DD, right)) :-
    % Conditions for valid crossing
    DI is CI - 2,
    DD is CD + 2,
    DI >= 0,
    DD =< 3,
    (MI = 0; MI >= DI),
	(MD = 0; MD >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover

% Define rules for two people to travel (one of each type)
% If a missionary and a cannibal cross, all numbers are modified by incrementing and decrementing by one accordingly.
valid(boat(1, 1, left),
    miscan(MI, CI, MD, CD, right),
    miscan(NI, DI, ND, DD, left)) :-
    % Conditions for valid crossing
    ND is MD - 1,
    DD is CD - 1,
    NI is MI + 1,
    DI is CI + 1,
    ND >= 0,
    DD >= 0,
    NI =< 3,
    DI =< 3,
    (NI = 0; NI >= DI),
    (ND = 0; ND >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
valid(boat(1, 1, right),
    miscan(MI, CI, MD, CD, left),
    miscan(NI, DI, ND, DD, right)) :-
    % Conditions for valid crossing
    NI is MI - 1,
    DI is CI - 1,
    ND is MD + 1,
    DD is CD + 1,
    NI >= 0,
    DI >= 0,
    ND =< 3,
    DD =< 3,
    (NI = 0; NI >= DI),
    (ND = 0; ND >= DD).
%(misionero,canibal, a donde va)
% estimo si es que los canibales no se pueden comer a los misioneros
% estimo si es que los canibales no se pueden comer a los misioneros
%misioneros = canibales  en el bote y se quita de donde esataba
 %canibales = misioneros en el bote y se quita de donde esataba
%inicio 
%fin
% si se cumple se mueve
% si se cumple se puede mover
% Define a helper predicate to check if an element is not in a list
not_in(_, []).
not_in(X, [Y | Rest]) :-
    X \= Y,
    not_in(X, Rest).
