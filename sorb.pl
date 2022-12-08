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

% 24 permutaiotns are possible for 4 elements?
reservation --> persons , time , date , menu.

persons --> [for] ,number_of_people.
number_of_people --> [X], {integer(X), X < 5}.

time --> [at] ,req_time.

req_time --> start_time.
start_time --> [X], {integer(X)}.

date --> [on] ,req_date.
req_date --> [X], {integer(X)}.

menu --> [standard, menu].
menu --> [theatre, menu].