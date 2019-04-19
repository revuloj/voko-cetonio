/* -*- Mode: Prolog -*- */
:- module(sqlrevo,[
	      search_eo/2,
	      search_trd/2,
	      search_eo_html/2,
	      search_eo_json/2,
	      search_trd_html/2,
	      editor_by_openid/2,
	      editor_by_subid/2,
	      editor_by_email/2,
	      editor_add/2,
	      editor_update/2,
	      editor_update_openid/2,
	      editor_update_subid/2
	  ]).

:- use_module(library(sha)).
:- use_module(library(http/http_open)).
:- use_module(library(prosqlite)).

:- use_module(agordo).
:- use_module(param_checks).

%revodb('/home/revo/tmp/inx_tmp/sql/revo.db').

:- initialization(connect).

/***
CREATE TABLE redaktanto (
  red_id VARCHAR(20) PRIMARY KEY NOT NULL,
  nomo VARCHAR(100) NOT NULL,
  openid TEXT,
  subid TEXT
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
  r.openid as openid,
  p.retposhto as retposhto
FROM redaktanto r, retposhto p
WHERE r.red_id=p.red_id
  AND p.numero = 1;
***/

connect :-
    agordo:get_config(revodb,RvDBFile),
    sqlite_connect(RvDBFile,revodb,[ext(''),alias(revodb)]),
    agordo:get_config(kontodb,KtDBFile),
    sqlite_connect(KtDBFile,kontodb,[ext(''),alias(kontodb)]).

download :-
    agordo:get_config(revodb_zip,UrlPattern),
    agordo:get_config(revodb_tmp,TmpFile),
    get_time(Time),
    once((
	    member(Diff,[0,1,2,3,4,5,6,7]),
	    (
		TimeD is Time - Diff * 24 * 3600,
		format_time(atom(DateStr),'%Y-%m-%d',TimeD),
		format(atom(Url),UrlPattern,[DateStr]),
		download_file(Url,TmpFile)%,
		%! % exit after first success
	    )
	)).


download_file(Url,File) :-
    setup_call_cleanup(
	    (
		catch(
			http_open(Url,InStream,[encoding(octet)]),
			_E,
			(format('~w not found.~n',[Url]), fail)),
		open(File,write,OutStream,[encoding(octet)])
	    ),
	    (
		debug(redaktilo(download),'downloading ~q to ~q',[Url,File]),
                format('downloading ~q to ~q',[Url,File]),
		copy_stream_data(InStream,OutStream)
	    ),
	    (
		close(InStream),
		close(OutStream)
	    )
	).

/******** serĉoj en Revo-datumbazo ***/

search_eo(Kion,Row) :-
    check_search(Kion),
    format(atom(Query),'select kap,num,art,mrk from nodo where kap like ''~w%'' collate nocase order by kap collate nocase, num',[Kion]),
     debug(sqlrevo,'query=~q',[Query]),
    sqlite_query(revodb,Query,Row).
%    sqlite_disconnect().

search_eo_limit(Kion,Row,Limit) :-
    check_search(Kion),
    format(atom(Query),
	   'select kap,num,art,mrk from nodo where kap like ''~w%'' order by kap collate nocase, num limit ''~d''',[Kion,Limit]),
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

homonimoj_sen_ref(Homonimoj) :-
   % format(atom(Query),'select distinct a.kap, a.art, b.art from nodo a, nodo b where a.kap=b.kap and a.art <> b.art and not exists (select * from referenco r where r.tip=''hom'' and r.mrk = a.mrk and r.cel = b.mrk) order by a.kap;',[]),
   format(atom(Query),'select distinct a.kap, a.art, b.art from nodo a, nodo b where a.kap=b.kap collate nocase and a.art <> b.art and not exists (select * from referenco r where r.tip=''hom'' and max(instr(r.mrk,a.mrk),instr(a.mrk,r.mrk))=1 and max(instr(r.cel,b.mrk),instr(b.mrk,r.cel))=1) order by a.kap;',[]),
  sqlite_query(revodb,Query,Homonimoj).

homonimoj_sen_ref_json(json([kap=Kap,art1=Art1,art2=Art2])) :-
    homonimoj_sen_ref(row(Kap,Art1,Art2)).


/**** serĉoj en redaktanto-datumbazo ***/

editor_by_openid(OpenId,Editor) :-
    check_url(OpenId),
    format(atom(Query),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where openid=''~w'';',[OpenId]),
    sqlite_query(kontodb,Query,Editor).

editor_by_subid(SubId,Editor) :- % openId Conenct
    check_int(SubId),
    format(atom(Query),'select red_id,nomo,retposhto from _redaktanto_poshto_unu where subid=''~w'';',[SubId]),
    sqlite_query(kontodb,Query,Editor).

editor_by_email(Email,Editor) :-
    check_email(Email,EmailChecked),
    format(atom(Sel1),'select red_id from retposhto where retposhto=''~w'';',[EmailChecked]),
    sqlite_query(kontodb,Sel1,row(RedId)),
    format(atom(Sel2),'select red_id,nomo,openid,retposhto from _redaktanto_poshto_unu where red_id=''~w'';',[RedId]),
    sqlite_query(kontodb,Sel2,Editor).

editor_by_redid(RedId,Editor) :-
    check_hash(RedId),
    format(atom(Query),'select red_id,nomo,openid,retposhto from _redaktanto_poshto_unu where red_id=''~w'';',[RedId]),
    sqlite_query(kontodb,Query,Editor).

editor_emails(RedId,Emails) :-
    check_hash(RedId),
    format(atom(Query),'select red_id,numero,retposhto from retposhto where red_id=''~w'';',[RedId]),
    findall(
	    No-Email,
	    sqlite_query(kontodb,Query,row(_,No,Email)),
	    Emails).


/**** aktualigoj de redaktanto-datumbazo ***/

editor_add(Nomo,[Email|Emails]) :-
    email_redid(Email,RedId),
    replace_apos(Nomo,N),
    format(atom(Ins),'insert into redaktanto (red_id,nomo) values ("~w","~w");',[RedId,N]),
    debug(redaktilo(redaktantoj),Ins,[]),
    sqlite_query(kontodb,Ins,row(1)),
    emails_add(RedId,1,[Email|Emails]).

emails_add(_,_,[]).
emails_add(RedId,No,[Email|Emails]) :-    
    format(atom(Ins),'insert into retposhto(red_id,numero,retposhto) values (''~w'',''~d'',''~w'');',[RedId,No,Email]),
    sqlite_query(kontodb,Ins,row(1)),
    No_1 is No+1,
    emails_add(RedId,No_1,Emails).


editor_update(Nomo,[Email|Emails]) :-
    email_redid(Email,RedId),
    once((
	% jam ekzistas -> aktualigu
	editor_by_redid(RedId,row(RedId,_,_,_)),
	editor_update_nomo(RedId,Nomo),
	editor_update_emails(RedId,[Email|Emails])
	;
	% ne jam ekzistas -> aldonu     
	editor_add(Nomo,[Email|Emails])
    )).

editor_update_nomo(RedId,Nomo) :-
    replace_apos(Nomo,N),
    format(atom(Upd),'update redaktanto set nomo=''~w'' where red_id = ''~w'' and nomo <> ''~w'';',[N,RedId,N]),
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
    sqlite_query(kontodb,Del,row(_)).

num_emails_add(RedId,[No-Email|MoreToBeAdded]) :-
   format(atom(Ins),'insert into retposhto (red_id,numero,retposhto) values (''~w'',''~w'',''~w'');',[RedId,No,Email]),
   sqlite_query(kontodb,Ins,row(1)),
   num_emails_add(RedId,MoreToBeAdded).
num_emails_add(_,[]).		   

editor_update_openid(Email,OpenId) :-
    format(atom(Sel),'select red_id from retposhto where retposhto=''~w'';',[Email]),
    sqlite_query(kontodb,Sel,row(RedId)),
    format(atom(Upd),'update redaktanto set openid=''~w'' where red_id = ''~w'';',[OpenId,RedId]),
    sqlite_query(kontodb,Upd,row(1)).

editor_update_subid(Email,SubId) :-
    format(atom(Sel),'select red_id from retposhto where retposhto=''~w'';',[Email]),
    sqlite_query(kontodb,Sel,row(RedId)),
    format(atom(Upd),'update redaktanto set subid=''~w'' where red_id = ''~w'';',[SubId,RedId]),
    sqlite_query(kontodb,Upd,row(1)).


/**** helpo-predikatoj ***/


email_redid(Email,RedId) :-
    atom(Email),
    sha_hash(Email,X,[]),
    hash_atom(X,Hash),
    sub_atom(Hash,0,20,_,RedId).

email_redid(Email,RedId) :-
    var(Email),
    editor_by_redid(RedId,row(_,_,_,Email)).

number_key_emails([Email|MoreMails],No,[No-Email|MoreNumbered]) :-
    No_1 is No+1,
    number_key_emails(MoreMails,No_1,MoreNumbered).
number_key_emails([],_,[]).

/*
%check_eo(Word) :-
%    wildcard_match([a-zA-zĉĝĥĵŝŭĈĜĤĴŜŬ],Word)

check_url(Url) :-
   parse_url(Url,_) -> true
   ;
   throw(invalid_sql_search_param(Url)).

% FIXME: * ne rilatas al anaŭa signo, sed funkcias kiel en uniksa ŝelo (dosiernomoj)!

check_email(Email) :-
    once((
	wildcard_match('<[a-zA-Z_.0-9]*@[a-zA-Z_.0-9]*>',Email) 
	;
	wildcard_match('[a-zA-Z_.0-9]*@[a-zA-Z_.0-9]*',Email)
	;
	throw(invalid_sql_search_param(Email))
    )).

check_int(Integer) :-
    once((
	wildcard_match('[0-9]*',Integer) 
        ;
	throw(invalid_sql_search_param(Integer))
    )).

check_hash(Hash) :-
    atom_length(Hash,20),
    wildcard_match('[0-9a-f]*',Hash)
    -> true
    ; throw(invalid_sql_search_param(Hash)).

check_search(Kion) :-
    atom_codes(Kion,Codes),
    \+ memberchk(39,Codes), % apostroph: '
    \+ memberchk(92,Codes), % backslash: \
    \+ memberchk(59,Codes)  % semicolon: ;
    -> true
    ;
    throw(invalid_sql_search_param(Kion)).
*/
    
replace_apos(In,Out) :-
    replace_atom(In,'''','''''',Out).

replace_atom(In,From,To,Out) :-
    atom_codes(From,FromList),
    atom_codes(To,ToList),
    atom_codes(In,InList),
    replace(InList,FromList,ToList,OutList),
    atom_codes(Out,OutList).

replace(InList,From,To,OutList) :-
    append(From,Rest,InList),!,
    replace(Rest,From,To,RestOut),
    append(To,RestOut,OutList).

replace([X|RestIn],From,To,[X|RestOut]) :-
    replace(RestIn,From,To,RestOut).

replace([],_,_,[]).

