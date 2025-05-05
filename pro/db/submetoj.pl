/* -*- Mode: Prolog -*- */
:- module(db_submetoj,[
	      submeto_add/5,
	      submeto_update/3,
          submetoj_delete/0,          
          submeto_by_id/2,
          submetoj_by_state/2,
          submetoj_by_email/3
	  ]).

:- use_module(library(prosqlite)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(param_checks)).
:- use_module(pro(db/util)).

% provizore por testi...
revodb('/home/revo/tmp/inx_tmp/sql/submetoj.db').

:- initialization(connect).

konserv_tagoj(200). % 200 tagojn ni konservas, pli malnovajn forigas regule

/***
CREATE TABLE submeto (
  sub_id INTEGER PRIMARY KEY NOT NULL, -- implicite, do ne skribenda: AUTOINCREMENT 
  sub_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  sub_state VARCHAR(5) NOT NULL DEFAULT 'nov'
    CHECK (sub_state IN ('nov','trakt','arkiv','erar','ignor')), 
                                -- trakt' = traktata, 'arkiv' = akceptita/arkivita, 'eraro' = rifuzita pro eraro
                                -- ignor ni uzas por testado
  sub_email VARCHAR(50) NOT NULL,
  sub_cmd VARCHAR(20) NOT NULL DEFAULT 'redakto', -- 'aldono' por nova dosiero, principe eblus ankau forigo!
  sub_desc VARCHAR(255) NOT NULL, -- la ŝanĝpriskribo
  sub_type VARCHAR(20) NOT NULL DEFAULT 'xml', -- 'zip/xml' por kunpremita xml
  sub_fname VARCHAR(50) NOT NULL, -- ĉe 'aldono' la nomo de nova dosiero
  sub_content TEXT NOT NULL,
  sub_result TEXT
);
***/

connect :-
    agordo:get_config(submetodb,SmDBFile),
    sqlite_connect(SmDBFile,submetodb,[ext(''),alias(submetodb)]).

/**** skribo de submeto ****/

submeto_add(Email,Cmd,Desc,FName,HexContent) :-
    replace_apos(Desc,Dsc),
    %replace_apos(Content,Cnt),
    format(atom(Ins),'insert into submeto (sub_email,sub_cmd,sub_desc,sub_fname,sub_content) values (''~w'',''~w'',''~w'',''~w'',x''~w'');',[Email,Cmd,Dsc,FName,HexContent]),
    debug(db(submetoj),'~q',[Ins]),
    sqlite_query(submetodb,Ins,row(1)).

submeto_update(Id,State,Result) :-
    replace_apos(Result,Res),
    format(atom(Upd),'update submeto set sub_state=''~w'', sub_result=''~w'' where sub_id=''~w'';',[Id,State,Res]),
    debug(db(submetoj),'~q',[Upd]),
    sqlite_query(submetodb,Upd,_).

submetoj_delete :-
    konserv_tagoj(Tagoj),
    format(atom(Del),'delete from submeto where sub_time < date(''now'', ''-~d days'');',[Tagoj]),
    debug(db(submetoj),'~q',[Del]),
    sqlite_query(submetodb,Del,_).

/**** serĉoj en submeto-datumbazo ***/

submeto_by_id(Id,Submeto) :-
    format(atom(Query),'select sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname,sub_content from submeto where sub_id=''~w'';',[Id]),    
    debug(db(submetoj),'~q',[Query]),
    sqlite_query(submetodb,Query,Submeto).

submetoj_by_state(State,Listo) :-
    format(atom(Query),'select sub_id,sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname from submeto where sub_state=''~w'';',[State]),    
    debug(db(submetoj),'~q',[Query]),
    sqlite_query(submetodb,Query,Listo).

submetoj_by_email(Email,Listo,Limit) :-
    format(atom(Query),'select sub_id,sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname,sub_result from submeto where sub_email=''~w'' order by sub_time limit ~d;',[Email,Limit]),    
    debug(db(submetoj),'~q',[Query]),
    sqlite_query(submetodb,Query,Listo).


