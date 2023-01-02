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

% -> translate list of inputs with dcg


reservation(A,B,C,D) --> reservation_part(A), reservation_part(B), reservation_part(C), reservation_part(D).

reservation_part(A) --> no_information, information(A), no_information.
reservation_part(B) --> no_information, information(B), no_information.
reservation_part(C) --> no_information, information(C), no_information.
reservation_part(D) --> no_information, information(D), no_information.
reservation_part(_) --> [].

information(date(D)) --> date(D).
information(persons(P)) --> persons(P).
information(menu(M)) --> menu(M).
% information(menu(M)) --> menu(M).
information(time(T)) --> time(T).

no_information --> \+ persons(_), \+ time(_), \+ date(_), \+ menu(_).
no_information --> [].
no_information --> [_].
no_information --> [_], no_information.

persons(P) --> [for] ,number_of_people(P).
persons(P) --> number_of_people(P), [people].
persons(P) --> number_of_people(P), [of, us].
number_of_people(P) --> [P], {integer(P), P < 5}. % reservations not fitting one table(P<5) are not allowed.
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

res_request(3,1,18).

% dining(dnumber, menu)

:- block dining(-,-).
% table1
dining(1,1).
dining(1,2).
dining(2,1).
dining(2,2).
dining(3,1).
dining(3,2).
dining(4,1).

% table2
dining(5,1).
dining(5,2).
dining(6,1).
dining(6,2).
dining(7,1).
dining(7,2).
dining(8,1).

% table3
dining(9,1).
dining(9,2).
dining(10,1).
dining(10,2).
dining(11,1).
dining(11,2).
dining(12,1).

%number of tables
tables(3).

constrain_dinings([],[],[]).
constrain_dinings([Dining|Dinings],
    [Time, Menu | Variables],% variables to drive constrained search, wat we want to find out.
    [dining(Dnumber, Persons, Date, Time, Menu)|DiningList]):-
        dining(Dnumber, Menu),
        %res_request(Persons, Menu, Date),
        tables(MaxTables),
        Persons in 1..4,
        Date in 15..20,
        Time in 19..22,
        Menu in 1..2, %1h theatre menu eating time, standard menu 2h eating time.
        constrain_dinings(Dinings, Variables, DiningList).

link_dinings([]).
link_dinings([_]).
link_dinings( [dining(Dining1, Persons1, Date1, Time1, Menu1),
        dining(Dining2, Persons2, Date2, Time2, Menu2)|Dinings]):-
            Dining2 #> Dining1,
            %Time2 #> Time1,
            ( Menu1 #= 1 ) #<==> ( Time2 #= Time1 + 1 ),
            %( Menu1 #= 2 ) #==> ( Time2 #= Time1 + 2 ),
            link_dinings([dining(Dining2, Persons2, Date2, Time2, Menu2)|Dinings]).

book_tables(DiningList) :-
            length(DiningList, 12), % 4 theather menus times 3 tables, a maximum of 12 dinings per evening.
            length(Dinings, 12),
            all_different(Dinings), % do not double book a dining slot.
            constrain_dinings(Dinings, Variables, DiningList),
            link_dinings(DiningList),
            labeling([ffc],Variables).
    


% control order => table 1, table2, table3 or minimise opening hour, close after last, or minimise simultanous guests?


% reifiable (A #= B) #<==> R make r concrete(0 or 1)
% R #= R1 + R2 + R3


% table(number, persons, time, date, menu)





