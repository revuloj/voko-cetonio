/* -*- Mode: Prolog -*- */

:- module(redaktantoj,[
    redk_listo/1 % text|json
    ]).

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

user:file_search_path(pro, './pro'). % aŭ: current_prolog_flag(home, Home). ...
:- use_module(pro(db/redaktantoj)).
:- use_module(pro(cfg/agordo)).


redk_listo(text) :-
    debug(redaktanto(redk_listo),'redk_listo(text)',[]),
    format('Content-type: text/plain; charset=utf-8~n~n'),
    forall(
        editor_list(row(_,Nomo,Retadresoj)),
        (
            format('~w,~w',[Nomo,Retadresoj])
        )
    ).


redk_listo(json) :-
    debug(redaktanto(redk_listo),'redk_listo(json)',[]),
    %[
    %    {"red_id":"1","red_nomo":"Wolfram Diestel","retadr":["wolfram@steloj.de","diestel@steloj.de"]},
    %    ...
    %]
    findall(_{
        red_id: Id, red_nomo: Nomo, retadr: LRetAdr},
        (
            editor_list(row(Id,Nomo,RetAdr)),
            atomic_list_concat(LRetAdr,',',RetAdr)
        ),
        Redaktantoj),
    reply_json(Redaktantoj).


update_redaktantoj :-
    parse_redaktantoj(Redoj),
    length(Redoj,L), L>10,
    update_redaktantoj(Redoj).

update_redaktantoj([]).
update_redaktantoj([Nomo-Emails|Redoj]) :-
    editor_update(Nomo,Emails),
    update_redaktantoj(Redoj).


parse_redaktantoj(Redoj) :-
    agordo:get_path(root_dir,redaktantoj_import,InFile),
    read_file_to_codes(InFile,Codes,[encoding(text)]),
    phrase(redaktantoj(Redoj),Codes).


redaktantoj([Red|Redoj]) -->
    redaktanto(Red), "\n", !,
    redaktantoj(Redoj).

redaktantoj([]) --> blanks, [].

redaktanto(Nomo-Retadresoj) -->
    nomo(Nomo), whites, retadresoj(Retadresoj), whites,
    {
	Nomo \= '',
	Retadresoj \= [],
%,
%	format('~q ~q~n',[Nomo,Retadresoj])
    debug(redaktantoj,'~q ~q~n',[Nomo,Retadresoj])
    }. % debug

nomo(Nomo) --> string_without("<",N),
	       {
		   atom_codes(N1,N),
		   normalize_space(atom(Nomo),N1)
	       }.

retadresoj([Adr|Adresoj]) -->
    retadreso(Adr), whites,
    retadresoj(Adresoj).
retadresoj([]) --> [].

retadreso(Adr) --> "<", string_without("@",Before), "@",  string_without(">",After), ">",
		   {
		       atom_codes(B,Before),
		       atom_codes(A,After),
		       atomic_list_concat([B,A],'@',Adr)
		   }.
    
