/* use these as input

reservation(A, B, C, D, [table,for,2,at,20,’:’,00,on,18,march],[]).
reservation(A, B, C, D, [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],[]).
reservation(A, B, C, D, [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).
reservation(A, B, C, D, [can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],[]).
reservation(A, B, C, D, [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],[]).
reservation(A, B, C, D, [9,people,on,18,th,of,march],[]).
reservation(A, B, C, D, [book,6,of,us,in,on,18,march,at,20,’:’,00],[]).
reservation(A, B, C, D, [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock],[]).

*/
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
number_of_people(P) --> [P], {integer(P), P < 5}. % reservation not fitting one table are not allowed.
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
% Opens @19h, closes @23h
% 3 tables, one table can seat 2,3,or 4.
% eat time = 1h for theatre menu, 2h for std menu
% the restaurant optimizes bookings, eg: prefer max persons

book_tables(DiningList) :-
    length(DiningList, 12), % 4 theather menus times 3 tables.
    length(Dinings, 12),
    all_different(Dinings),
    constrain_dinings(Dinings, Variables, DiningList),
    link_dinings(DiningList),
    labeling([ffc],Variables).

constrain_time(T):-
T in 19..22.

constrain_table_number(N):-
    N in 1..3.

% table(table_number, persons, time, date, menu)
% :- block table(-,-,-,-,-).

tables(3).

table(1, Persons, Time, Date, Menu).
table(2, Persons, Time, Date, Menu).
table(3, Persons, Time, Date, Menu).

% dining(persons, time, date, menu)

dining(1,1).
dining(1,2).

% should recieve this from dcg.
dining(Dnumber, Persons, Time, Date, Menu).

constrain_dinings([],[],[]).
constrain_dinings([Dining|Dinings],
    [Time, Menu, Table | Variables],% variables to drive constrained search, wat we want to fin out.
    [dining(Dnumber, Persons, Date, Time, Menu, Table)|DiningList]):-
        dining(Dnumber, Persons, Time, Date, Menu),
        tables(MaxTables),
        Time in 19..22,
        Menu in 1..2, %1h theatre menu eating time, standard menu 2h eating time.
        Table in 1..3,
        constrain_dinings(Dinings, Variables, DiningList).

link_dinings([]).
link_dinings([_]).
link_dinings( [dining(_, Persons1, Date1, Time1, Menu1, Table1),
        dining(Dining2, Persons2, Date2, Time2, Menu2, Table2 )|Dinings]):-
            Table2 #>= Table1,
            Table2 #=< Table1 + 1,
            (Table1 #= Table2) #==> (Time1 #\ Time2),
            link_dinings([dining(Dining2, Persons2, Date2, Time2, Menu2, Table2 )|Dinings]).


    


constrain_tables([],[],[]).
constrain_tables([Table|Tables], [Persons, Time, Date, Menu |Variables],[table(Number, Persons, Time, Date, Menu)|TableList]):-
    Number in 1..3,
    Person in 2..4,
    Time in 19..22,
    Date in 1..31,
    Menu in 1..2,% eats 1hour or 2 hours.
    constrain_tables(Tables, Variables, TableList).

% control order => table 1, table2, table3 or minimise opening hour, close after last, or minimise simultanous guests?


% reifiable (A #= B) #<==> R make r concrete(0 or 1)
% R #= R1 + R2 + R3


% table(number, persons, time, date, menu)





