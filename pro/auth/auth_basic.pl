/* -*- Mode: Prolog -*- */
:- module(auth_basic,
	  [
	  ]).

%:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_session)).
%:- use_module(library(http/http_header)).
:- use_module(library(http/http_authenticate)).

:- use_module(library(debug)).

%:- use_module(pro(cfg/agordo)).

:- multifile http:authenticate/3.

/** <module> auth_basic
 * 
 * Tiu ĉi modulo ebligas aŭtentigi HTTP-petojn per BasicAuth (uzanto/pasvorto).
 * Ni uzas tion por ke voko-afido povu ricevi kaj aktualigi submetojn.
*/

http:authenticate(basic,Request,[user(User)]) :- basic_auth(Request,User).

basic_auth(Request,User) :-
    agordo:get_path(root_dir,passwd,Passwd),
    (exists_file(Passwd) 
    -> true
    % mankas la pasvort-dosiero (vd bin/instalo.sh subm-pwd)
    ; throw(http_reply(server_error('baza salutado ne agordita!')))
    ),
    % kontrolu la pasvorton
    (http_authenticate(basic(Passwd), Request, [User|_])
    ->  true % (debug(auth,'~q',[User]), true)
    ; throw(http_reply(authorise(basic, 'submetoj')))
    ).    
