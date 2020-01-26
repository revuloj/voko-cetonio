/* -*- Mode: Prolog -*- */
:- module(auth_ajax,
	  [
	      request_ajax_id/2,
	      ajax_id_time_valid/1,
	      new_ajax_id/2
	  ]).

%:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_parameters)). % reading post data
:- use_module(library(http/http_session)).
:- use_module(library(http/http_header)).
%:- use_module(library(http/http_host)).
%:- use_module(library(http/html_write)).
%:- use_module(library(http/json)).
%:- use_module(library(settings)).

:- use_module(library(debug)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(db/redaktantoj)).
%:- use_module(pro(param_checks)).

% iel aŭtomata utf8 ne funkcias kiam redaktilo-test 
% lanĉiĝas kiel systemd servo (per sistemestro "root")
% eble pro LC_LANG medio-variablo aŭ simile?
:- encoding(utf8).

:- multifile http:authenticate/3.
http:authenticate(ajaxid,Request,[user(User),email(Email)]) :-  ajax_auth(Request,User,Email).


ajax_auth(Request,RedID,Email) :-
	once((
		ajax_user(Request,RedID,ClientIP,Email),
		debug(redaktilo(ajaxid),'AjaxAuth: ~q ~q',[RedID,ClientIP])
		;
		% kreu AjaxID el session+request
		new_ajax_id_cookie(Request,RedID,Cookie),
		debug(redaktilo(ajaxid),'AjaxID: ~q',[Cookie]),
		format('Set-Cookie: ~w\r\n',[Cookie])
		; 
		% Ajax-hash ne (plu) valida, necesas resaluto
		format('Status: ~d~n~n',[401]),
		throw(http_reply(html([': Tro longa tempo pasis post saluto, necesas resaluti nun.\n'])))
    )). 

% identigo per kuketo AjaxID, kiu konsistas el redaktanto ID kaj kuketo-kreo-tempo signitaj per HMAC
% en la kalkulado de HMAC eniras ankaŭ la klienta IP, tiel ke ĝi ne validas por alia kliento se li
% ŝtelis ĝin

ajax_user(Request,RedID,ClientIP,Retadreso) :-
    %debug(redaktilo(ajaxid),'AjaxRequest ~q',[Request]),
    member(peer(ip(A,B,C,D)),Request),
    % debug(redaktilo(ajaxid),'AjaxID_a ~q',[Cookies]),
    atomic_list_concat([A,B,C,D],'.',ClientIP),
    %debug(redaktilo(ajaxid),'AjaxID_b ~q',[ClientIP]),
    request_ajax_id(Request,AjaxID),
    %debug(redaktilo(ajaxid),'AjaxID_c ~q ~q',[AjaxID,ClientIP]),
    sub_atom(AjaxID,0,20,_,RedID),
    sub_atom(AjaxID,20,8,_,HexTime),
    sub_atom(AjaxID,28,20,0,HexMac),
    %debug(redaktilo(ajaxid),'AjaxID_d ~q ~q ~q',[RedID,HexTime,HexMac]),
    % is the cookie still valid?
    ajax_id_time_valid(AjaxID),
    % is the hmac hash valid?
    ajax_hmac(RedID,ClientIP,HexTime,HexMac),
    % save in session if already closed
	db_redaktantoj:email_redid(Retadreso,RedID).

ajax_id_time_valid(AjaxID) :-
    sub_atom(AjaxID,20,8,_,HexTime), 
    get_time(Now), hex_value(HexTime,Time),
    Valid is Time + 24 * 3600,
    %http_timestamp(Valid,ValidTill),
    Valid > Now.

request_ajax_id(Request,AjaxID) :-
    once((
		% normale AjaxID estu en la kuketo
		member(cookie(Cookies),Request),
		member(redaktilo_ajax=AjaxID,Cookies)
		;
		% permesu AjaxID ankaŭ kiel URL-parametro, ekz. por elprovi la citajho-serchon     
		member(search(Search),Request),
		member(redaktilo_ajax=AjaxID,Search)
    )).

/******************************* helpopredikatoj **********************/

hex_value(Hex,Val) :-
	atom_codes(Hex,Codes),
	hex_value_(Codes,_,Val).
    
hex_value_([H|T],Shift,Val) :-
    hex_value_(T,Sh,V),
    (H =< 57 -> D is H-48 ; D is H - 87),
    Val is D << Sh \/ V,
    Shift is Sh + 4.

hex_value_([],0,0).
	     
new_ajax_id(Request,AjaxID) :-
    new_ajax_id(Request,_,AjaxID,_).
	   
new_ajax_id(Request,RedId,AjaxID,Time) :-
    % get client information
    %debug(redaktilo(ajaxid),'AjaxID_1 ~q',Request),
    member(peer(ip(A,B,C,D)),Request),
    %debug(redaktilo(ajaxid),'AjaxID_2 ~q ~q',[A,B]),
    atomic_list_concat([A,B,C,D],'.',ClientIP),
    http_session_data(retadreso(Retadreso)),
    db_redaktantoj:email_redid(Retadreso,RedId),
    debug(redaktilo(ajaxid),'AjaxID_3 ~q ~q',[RedId,Retadreso]),
    % calculate hmac
    get_time(Time), Seconds is floor(Time),
    format(atom(HexTime),'~16r',[Seconds]),
    ajax_hmac(RedId,ClientIP,HexTime,HexMac20),
    atomic_list_concat([RedId,HexTime,HexMac20],'',AjaxID).
    %debug(redaktilo(ajaxid),'AjaxID_6 ~q ~q',[String,AjaxID]),
 
new_ajax_id_cookie(Request,RedID,Cookie) :-
    new_ajax_id(Request,RedID,AjaxID,Time),	
    setting(http:prefix,Prefix),
    Time24 is Time + 240 * 3600,
    http_timestamp(Time24,Expires),
    format(atom(Cookie),'redaktilo_ajax=~w;Expires=~w;Path=~w; Version=1',
	   [AjaxID,Expires,Prefix]).

ajax_hmac(RedId,ClientIP,HexTime,HexMac20) :-
    %debug(redaktilo(ajaxid),'AjaxID_hmac ~q ~q ~q',[RedId,ClientIP,HexTime]),
    atomic_list_concat([RedId,ClientIP,HexTime],String),
    agordo:get_config(ajax_secret,AjaxSecret),
    %debug(redaktilo(ajaxid),'AjaxID_hmac ~q ~q',[String,AjaxSecret]),
    hmac_sha(AjaxSecret,String,HMac,[algorithm(sha256)]),
    hash_atom(HMac,Hex), sub_atom(Hex,0,20,_,HexMac20).


