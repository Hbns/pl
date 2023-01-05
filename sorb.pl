/* use these as input

list_of_inputs:

[[table,for,2,at,20,’:’,00,on,18,march],
[we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],
[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],
[reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]
[9,people,on,18,th,of,march],
[book,6,of,us,in,on,18,march,at,20,’:’,00],
[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]]

reservation(A, B, C, D, [table,for,2,at,20,’:’,00,on,18,march],[]).
reservation(A, B, C, D, [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],[]).
reservation(A, B, C, D, [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).
reservation(A, B, C, D, [can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],[]).
reservation(A, B, C, D, [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],[]).
reservation(A, B, C, D, [9,people,on,18,th,of,march],[]).
reservation(A, B, C, D, [book,6,of,us,in,on,18,march,at,20,’:’,00],[]).
reservation(A, B, C, D, [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock],[]).

*/
% test_dcg(+Input_phrase, -Output_list)
% test_dcg returns the input_phrase after applying the dcg as a list with relevant information.
test_dcg(Input_phrase, Output_list):-
    reservation(Output_list, Input_phrase, []).

% convert_input(+[Phrase|Phrases], -[Reservation|Reservations])
% Reservations is the dcg translated list of input phrases.    
convert_input([], []).
convert_input([Phrase|Phrases], [Reservation|Reservations]):-
    reservation(Reservation, Phrase, []),
    convert_input(Phrases, Reservations).

/*
convert_input([[table,for,2,at,20,’:’,00,on,18,march],
[we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],
[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],
[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],
[reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],
[9,people,on,18,th,of,march],
[book,6,of,us,in,on,18,march,at,20,’:’,00],
[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]], Answer).

*/

%% DCG %%

reservation([A, B, C, D]) --> reservation_part(A), reservation_part(B), reservation_part(C), reservation_part(D).

reservation_part(A) --> no_information, information(A), no_information.
reservation_part(B) --> no_information, information(B), no_information.
reservation_part(C) --> no_information, information(C), no_information.
reservation_part(D) --> no_information, information(D), no_information.
reservation_part(_) --> [].

information(date(D)) --> date(D).
information(persons(P)) --> persons(P).
information(menu(M)) --> menu(M).
information(time(T)) --> time(T).

no_information --> \+ persons(_), \+ time(_), \+ date(_), \+ menu(_).
no_information --> [].
no_information --> [_].
no_information --> [_], no_information.

persons(P) --> [for] ,number_of_people(P).
persons(P) --> number_of_people(P), [people].
persons(P) --> number_of_people(P), [of, us].
number_of_people(P) --> [P], {integer(P)}.
number_of_people(P) --> [a, party, of], number_of_people(P).

% preferably? 
time(T) --> [at] ,req_time(T).
req_time(T) --> [T], {integer(T)}.

date(D) --> [on] ,req_date(D).
req_date(D) --> [D], {integer(D)}.
req_date(D) --> [_Word], req_date(D).

menu(M) --> [M, menu].



%% Constraint System %%
:- use_module( [library(clpfd),library(lists)] ).
% to use block, we need:
:- expects_dialect( sicstus ).

% Opens @19h, closes @23h
% 3 tables, one table can seat 2,3,or 4.
% eat time = 1h for theatre menu, 2h for standard menu
% the restaurant optimizes bookings, eg: prefer max persons

% the constrained system was based around the box stacking example in the lecture slides.
% I did not understand how I could do this input_phrase per input_phrase.
% the resulting solution is not general, eg: I have to specify how many correct answers there will be.

test_constraints():-
    book_tables(_).

% "received" dining requests

:- block res_request(-,-,-,-,-).

% there are 4 valid reservations (where 1 < persons < 5)
res_request(1,2,20,18,_).
res_request(2,5,8,18,_).
res_request(3,3,_,18,1).
res_request(4,2,_,18,2).
res_request(5,4,_,18,2).
res_request(6,9,_,18,_).
res_request(7,6,20,18,_).
res_request(8,7,7,18,2).

constrain_dinings([],[],[]).
constrain_dinings([Dining|Dinings],
    [Time, Menu | Variables],% variables to drive constrained search, wat we want to find out.
    [dining(Dnumber, Persons, Date, Time, Menu)|DiningList]):-
        res_request(Dnumber, Persons, Time, Date, Menu),
        %Table in 1..3,
        Persons in 2..4,
        Time in 19..22,
        Menu in 1..2, %1h theatre menu eating time, standard menu 2h eating time.
        constrain_dinings(Dinings, Variables, DiningList).

link_dinings([]).
link_dinings([_]).
link_dinings( [dining(Dining1, Persons1, Date1, Time1, Menu1),
        dining(Dining2, Persons2, Date2, Time2, Menu2)|Dinings]):-
            Dining2 #> Dining1, % order the dinings.
            ( Menu1 #= 2 ) #<==> ( Time1 #< 22), % when choosing menu 2, latest dinner time is 21h.
            % not sure how to constrain a maximum of 3 similar Time slots, since only 3 tables??
            link_dinings([dining(Dining2, Persons2, Date2, Time2, Menu2)|Dinings]).

book_tables(DiningList) :-
            length(DiningList, 4), % must be number of valid requests, use list size?
            length(Dinings, 4),
            all_distinct(Dinings),
            constrain_dinings(Dinings, Variables, DiningList),
            link_dinings(DiningList),
            labeling([ffc],Variables).
   
% returned by book_tables:
% DiningList = [dining(1, 2, 18, 20, 2), dining(3, 3, 18, 22, 1), dining(4, 2, 18, 19, 2), dining(5, 4, 18, 19, 2)]

% print_reservations(+Dinings)
% given a list of dinings, each dining will be printed on a new line.
print_reservations([]).
print_reservations([Dining|Dinings]):-
    dining(Dnumber, Persons, Date, Time, Menu) = Dining,
    format("~w~w~w~w~w~w~w~w~w~w~n", ["Reservationnumber: ",Dnumber,", Date: ", Date, ", Time: ", 
        Time, ", Persons: ", Persons, ", Menu: ", Menu]),
    print_reservations(Dinings).

% test_all() runs phase 2 and 3 of the programm.
test_all():-
    % DCG
    % the information to the book_tables constraints are facts in this programm.
    % Constraints
    book_tables(DiningList),
    % Display
    print_reservations(DiningList).



