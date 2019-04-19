select * from traduko where lng='de' and mrk like 'favor.%';

% mankantaj tradukoj
select * from nodo where art = 'favor' and not exists (select * from traduko where lng='de' and mrk=nodo.mrk);

% tradukoj de artikolo
select nodo.mrk,traduko.trd from nodo,traduko where art = 'favor' and traduko.lng='de' and traduko.mrk=nodo.mrk;

% ambau per left outer join
select nodo.mrk,nodo.kap,nodo.num,traduko.trd from nodo left outer join traduko on traduko.mrk=nodo.mrk and traduko.lng='de' where art = 'favor';

