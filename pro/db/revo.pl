/* -*- Mode: Prolog -*- */
:- module(db_revo,[
	      search_eo/2,
	      search_trd/2,
	      search_eo_html/2,
	      search_eo_json/2,
	      search_trd_html/2
          /*,
          homonimoj_sen_ref/1,
          homonimoj_sen_ref_json/1*/
	  ]).

:- use_module(library(prosqlite)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(param_checks)).

%revodb('/home/revo/tmp/inx_tmp/sql/revo.db').

:- initialization(connect).

/***
CREATE TABLE redaktanto (
  red_id VARCHAR(20) PRIMARY KEY NOT NULL,
  nomo VARCHAR(100) NOT NULL,
  openid TEXT,
  subid_gg TEXT,
  subid_yh TEXT,
  subid_fb TEXT,
  subid_gh TEXT
);

CREATE TABLE retposhto (
  red_id VARCHAR(20) NOT NULL,
  numero INTEGER NOT NULL,
  retposhto VARCHAR(100) NOT NULL,
  PRIMARY KEY (red_id, numero)
);

CREATE VIEW _redaktanto_poshto_unu AS
SELECT
  r.red_id as red_id,
  r.nomo as nomo,
  r.subid_gg as subid_gg,
  r.subid_yh as subid_yh,
  r.subid_fb as subid_fb,
  r.subid_gh as subid_gh,
  p.retposhto as retposhto
FROM redaktanto r, retposhto p
WHERE r.red_id=p.red_id
  AND p.numero = 1;

***/

connect :-
    agordo:get_config(revodb,RvDBFile),
    sqlite_connect(RvDBFile,revodb,[ext(''),alias(revodb)]).


/******** serĉoj en Revo-datumbazo ***/

search_eo(Kion,Row) :-
    check_search(Kion),
    format(atom(Query),'select kap,num,art,mrk from _kap where kap like ''~w%'' collate nocase order by kap collate nocase, num',[Kion]),
     debug(sqlrevo,'query=~q',[Query]),
    sqlite_query(revodb,Query,Row).
%    sqlite_disconnect().

search_eo_limit(Kion,Row,Limit) :-
    check_search(Kion),
    format(atom(Query),
	   'select kap,num,art,mrk from _kap where kap like ''~w%'' order by kap collate nocase, num limit ''~d''',[Kion,Limit]),
    debug(sqlrevo,'query=~q',[Query]),
    sqlite_query(revodb,Query,Row).

search_trd(Kion,Row) :-
    check_search(Kion),
    format(atom(Query),'select traduko.trd,nodo.kap,traduko.lng,nodo.art,traduko.mrk from traduko,nodo where traduko.trd like ''~w%'' and traduko.mrk = nodo.mrk;',[Kion]),
    debug(sqlrevo,'query=~q',[Query]),
    sqlite_query(revodb,Query,Row).
%    sqlite_disconnect().

search_eo_html(Kion,Html) :-
    search_eo(Kion,row(Kap,Num,Art,Mrk)),
    format(atom(Html),'<a href="~w.html#~w">~w ~w</a>',[Art,Mrk,Kap,Num]).

search_eo_json(Kion,json([kap=Kap,num=Num,art=Art,mrk=Mrk])) :-
    search_eo_limit(Kion,row(Kap,Num,Art,Mrk),100).


search_trd_html(Kion,Html) :-
    search_trd(Kion,row(Trd,Eo,Lng,Art,Mrk)),
    format(atom(Html),'<a href="~w.html#~w">~w: ~w (~w)</a>',[Art,Mrk,Lng,Trd,Eo]).

art_trd(Artikolo,Lng,Tradukoj) :-
    check_search(Artikolo),
    format(atom(Query),'select nodo.mrk,nodo.kap,nodo.num,traduko.trd from nodo left outer join traduko on traduko.mrk=nodo.mrk and traduko.lng=''~w'' where art = ''~w'';',[Lng,Artikolo]),
    debug(sqlrevo,'query=~q',[Query]),
    sqlite_query(revodb,Query,Tradukoj).

art_trd_json(Artikolo,Lng,json([mrk=Mrk,kap=Kap,num=Num,trd=Trd])) :-
    art_trd(Artikolo,Lng,row(Mrk,Kap,Num,Trd)).

/*
homonimoj_sen_ref(Homonimoj) :-
   % format(atom(Query),'select distinct a.kap, a.art, b.art from nodo a, nodo b where a.kap=b.kap and a.art <> b.art and not exists (select * from referenco r where r.tip=''hom'' and r.mrk = a.mrk and r.cel = b.mrk) order by a.kap;',[]),
   format(atom(Query),'select distinct a.kap, a.art, b.art from nodo a, nodo b where a.kap=b.kap collate nocase and a.art <> b.art and not exists (select * from referenco r where r.tip=''hom'' and max(instr(r.mrk,a.mrk),instr(a.mrk,r.mrk))=1 and max(instr(r.cel,b.mrk),instr(b.mrk,r.cel))=1) order by a.kap;',[]),
  sqlite_query(revodb,Query,Homonimoj).

homonimoj_sen_ref_json(json([kap=Kap,art1=Art1,art2=Art2])) :-
    homonimoj_sen_ref(row(Kap,Art1,Art2)).
*/


