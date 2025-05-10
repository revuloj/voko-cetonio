/* -*- Mode: Prolog -*- */
:- module(redaktanto_srv,
	  [ 
        redk_listo/1 % text|json
	  ]).

:- use_module(library(debug)).
%:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(pro(db/redaktantoj)).


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
