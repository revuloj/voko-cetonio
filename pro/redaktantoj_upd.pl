/* -*- Mode: Prolog -*- */

:- module(redaktantoj,[]).

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

user:file_search_path(pro, './pro'). % aŭ: current_prolog_flag(home, Home). ...
:- use_module(pro(db/redaktantoj)).
:- use_module(pro(cfg/agordo)).


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
    
