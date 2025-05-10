/* -*- Mode: Prolog -*- */
:- module(db_redaktantoj,[
	      editor_by_subid/3,
	      editor_by_email/2,
          editor_list/1,
	      editor_add/2,
	      editor_update/2,
          editor_update_subid/3,
          email_redid/2
	  ]).

:- use_module(library(sha)).
:- use_module(library(prosqlite)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(param_checks)).
:- use_module(pro(db/util)).

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
    agordo:get_config(kontodb,KtDBFile),
    sqlite_connect(KtDBFile,kontodb,[ext(''),alias(kontodb)]).


/**** serĉoj en redaktanto-datumbazo ***/

%% editor_by_openid(OpenId,Editor) :-
%%     check_url(OpenId),
%%     format(atom(Query),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where openid=''~w'';',[OpenId]),
%%     sqlite_query(kontodb,Query,Editor).

col_suffix(google,gg).
col_suffix(yahoo,yh).
col_suffix(facebook,fb).
col_suffix(github,gh).

editor_by_subid(SubId,Provider,Editor) :- % Oauth2 / OpenId Connect
    %% ĉe yahoo ĝi enhavas literojn! check_int(SubId),
    check_search(SubId),
    col_suffix(Provider,Sfx),
    format(atom(Query),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where subid_~w=''~w'';',[Sfx,SubId]),    
    debug(db(redaktantoj),'~q',[Query]),
    sqlite_query(kontodb,Query,Editor).

editor_by_email(Email,Editor) :-
    check_email(Email,EmailChecked),
    format(atom(Sel1),'select red_id from retposhto where retposhto=''~w'';',[EmailChecked]),
    debug(db(redaktantoj),'~q',[Sel1]),
    sqlite_query(kontodb,Sel1,row(RedId)),
    format(atom(Sel2),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where red_id=''~w'';',[RedId]),
    debug(db(redaktantoj),'~q',[Sel2]),
    sqlite_query(kontodb,Sel2,Editor).

editor_by_redid(RedId,Editor) :-
    check_hash(RedId),
    format(atom(Query),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where red_id=''~w'';',[RedId]),
    debug(db(redaktantoj),'~q',[Query]),
    sqlite_query(kontodb,Query,Editor).

editor_emails(RedId,Emails) :-
    check_hash(RedId),
    format(atom(Query),'select red_id,numero,retposhto from retposhto where red_id=''~w'';',[RedId]),
    debug(db(redaktantoj),'~q',[Query]),
    findall(
	    No-Email,
	    sqlite_query(kontodb,Query,row(_,No,Email)),
	    Emails).

editor_list(Editor) :-
    format(atom(Query),'select 1000 + row_number() over (order by r.red_id) as id, r.nomo as nomo, group_concat(p.retposhto,'','') as retadresoj from redaktanto r left join retposhto p on r.red_id = p.red_id group by r.red_id,r.nomo',[]),
    debug(db(redaktantoj),'~q',[Query]),
    sqlite_query(kontodb,Query,Editor).

/**** aktualigoj de redaktanto-datumbazo ***/

editor_add(Nomo,[Email|Emails]) :-
    email_redid(Email,RedId),
    replace_apos(Nomo,N),
    format(atom(Ins),'insert into redaktanto (red_id,nomo) values ("~w","~w");',[RedId,N]),
    debug(db(redaktantoj),'~q',[Ins]),
    sqlite_query(kontodb,Ins,row(1)),
    emails_add(RedId,1,[Email|Emails]).

emails_add(_,_,[]).
emails_add(RedId,No,[Email|Emails]) :-    
    format(atom(Ins),'insert into retposhto(red_id,numero,retposhto) values (''~w'',''~d'',''~w'');',[RedId,No,Email]),
    debug(db(redaktantoj),'~q',[Ins]),
    sqlite_query(kontodb,Ins,row(1)),
    No_1 is No+1,
    emails_add(RedId,No_1,Emails).


editor_update(Nomo,[Email|Emails]) :-
    email_redid(Email,RedId),
    once((
        % jam ekzistas -> aktualigu
        editor_by_redid(RedId,row(RedId,_,_)),
        editor_update_nomo(RedId,Nomo),
        editor_update_emails(RedId,[Email|Emails])
	;
        % ne jam ekzistas -> aldonu     
        editor_add(Nomo,[Email|Emails])
    )).

editor_update_nomo(RedId,Nomo) :-
    replace_apos(Nomo,N),
    format(atom(Upd),'update redaktanto set nomo=''~w'' where red_id = ''~w'' and nomo <> ''~w'';',[N,RedId,N]),
    debug(db(redaktantoj),'~q',[Upd]),
    sqlite_query(kontodb,Upd,_).

editor_update_emails(RedId,Emails) :-
    number_key_emails(Emails,1,NumMails),
    editor_emails(RedId,DBMails),
    ord_subtract(DBMails,NumMails,ToBeDeleted),
    ord_subtract(NumMails,DBMails,ToBeAdded),
    num_emails_del(RedId,ToBeDeleted),
    num_emails_add(RedId,ToBeAdded).


num_emails_del(_,[]) :- !.
num_emails_del(RedId,ToBeDeleted) :-
    pairs_keys(ToBeDeleted,DelKeys),
    atomic_list_concat(DelKeys,''',''',InNoStr),
    format(atom(Del),'delete from retposhto where red_id=''~w'' and numero in (''~w'');',[RedId,InNoStr]),
    debug(db(redaktantoj),'~q',[Del]),
    sqlite_query(kontodb,Del,row(_)).

num_emails_add(RedId,[No-Email|MoreToBeAdded]) :-
   format(atom(Ins),'insert into retposhto (red_id,numero,retposhto) values (''~w'',''~w'',''~w'');',[RedId,No,Email]),
   debug(db(redaktantoj),'~q',[Ins]),
   sqlite_query(kontodb,Ins,row(1)),
   num_emails_add(RedId,MoreToBeAdded).
num_emails_add(_,[]).		   

%% editor_update_openid(Email,OpenId) :-
%%     format(atom(Sel),'select red_id from retposhto where retposhto=''~w'';',[Email]),
%%     sqlite_query(kontodb,Sel,row(RedId)),
%%     format(atom(Upd),'update redaktanto set openid=''~w'' where red_id = ''~w'';',[OpenId,RedId]),
%%     sqlite_query(kontodb,Upd,row(1)).

editor_update_subid(Email,Provider,SubId) :-
    %% ĉe yahoo ĝi enhavas literojn! check_int(SubId),
    check_search(SubId),
    col_suffix(Provider,Sfx),
    format(atom(Sel),'select red_id from retposhto where retposhto=''~w'';',[Email]),
    debug(db(redaktantoj),'~q',[Sel]),
    sqlite_query(kontodb,Sel,row(RedId)),
    format(atom(Upd),'update redaktanto set subid_~w=''~w'' where red_id = ''~w'';',[Sfx,SubId,RedId]),
    debug(db(redaktantoj),'~q',[Upd]),
    sqlite_query(kontodb,Upd,row(1)).


/**** helpo-predikatoj ***/


email_redid(Email,RedId) :-
    atom(Email),
    sha_hash(Email,X,[]),
    hash_atom(X,Hash),
    sub_atom(Hash,0,20,_,RedId).

email_redid(Email,RedId) :-
    var(Email),
    editor_by_redid(RedId,row(_,_,Email)).

number_key_emails([Email|MoreMails],No,[No-Email|MoreNumbered]) :-
    No_1 is No+1,
    number_key_emails(MoreMails,No_1,MoreNumbered).
number_key_emails([],_,[]).
