/* use these as input
[table,for,2,at,20,’:’,00,on,18,march]
[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th]
[we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03]
[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please]
[reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu]
[9,people,on,18,th,of,march]
[book,6,of,us,in,on,18,march,at,20,’:’,00]
[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock]
*/

% 24 permutations are possible for 4 elements?

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
% number_of_people(P) --> [P], {integer(P), P < 5}. % will do this in the constrains system?
number_of_people(P) --> [P], {integer(P)}.
number_of_people(P) --> [a, party, of], number_of_people(P).

% preferably?
time(T) --> [at] ,req_time(T).
req_time(T) --> [T], {integer(T)}.

date(D) --> [on] ,req_date(D).
req_date(D) --> [D], {integer(D)}.
req_date(D) --> [_Word], req_date(D).

menu(M) --> [M, menu].

/*
reservation(A, B, C, D, [table,for,2,at,20,’:’,00,on,18,march],[]).
reservation(A, B, C, D, [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],[]).
reservation(A, B, C, D, [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).
reservation(A, B, C, D, [can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],[]).
reservation(A, B, C, D, [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],[]).
reservation(A, B, C, D, [9,people,on,18,th,of,march],[]).
reservation(A, B, C, D, [book,6,of,us,in,on,18,march,at,20,’:’,00],[]).
reservation(A, B, C, D, [reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock],[]).
*/


