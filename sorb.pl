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

% reservation(P, T, D, [table,for,2,at,20,’:’,00,on,18,march],[]).
% reservation(P, T, D, [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,’/’,03],[]).
reservation(P, T, D) --> no_information, information(P), no_information, information(T), 
no_information, information(D), no_information.

% reservation(P, M, D, [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).
reservation(P,M, D) --> no_information, information(P), no_information, information(M), 
no_information, information(D), no_information.

% reservation(P, M, D,[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).
reservation(P, T, D) --> no_information, information(P), no_information, information(T), 
no_information, information(D), no_information.

% reservation(P, M, D,[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],[]).
reservation(T, P, D, M) --> no_information, information(T), no_information, information(P), 
no_information, information(D), no_information, information(M), no_information.

% reservation(D, P, M,[reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],[]).
reservation(D, P, M) --> no_information, information(D), no_information, information(P), 
no_information, information(M), no_information.

% reservation(P, D,[9,people,on,18,th,of,march],[]).
reservation(P, D) --> no_information, information(P), no_information, information(D),no_information.

% reservation(D, P, M,[book,6,of,us,in,on,18,march,at,20,’:’,00],[]).
reservation(P, D, T) --> no_information, information(P), no_information, information(D), 
no_information, information(T), no_information.

% reservation(P, D, M, T,[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock],[]).
reservation(P, D, M, T) --> no_information, information(P), no_information, information(D), 
no_information, information(M), no_information, information(T), no_information.



information(P) --> persons(P).
information(T) --> time(T).
information(D) --> date(D).
information(M) --> menu(M).

no_information --> \+ persons(_), \+ time(_), \+ date(_), \+ menu(_).
no_information --> [].
no_information --> [_].
no_information --> [_], no_information.

persons(P) --> [for] ,number_of_people(P).
person(P) --> number_of_people(P), [people].
person(P) --> number_of_people(P), [of, us].
number_of_people(P) --> [P], {integer(P), P < 5}.
number_of_people(P) --> [a, party, of], number_of_people(P).

time(T) --> [at] ,req_time(T).
req_time(T) --> [T], {integer(T)}.

date(D) --> [on] ,req_date(D).
req_date(D) --> [D], {integer(D)}.
req_date(D) --> [_Word], req_date(D).

menu(M) --> [M, menu].
menu(M) --> [M, menu].


% reservation(P, [for, 2, at, 20, on, 17, standard, menu],[]).
% reservation(P, T, D, M,[blibla, for, 2,and, at, 20, andso, indeed, on, 17, hello, standard, menu],[]).
