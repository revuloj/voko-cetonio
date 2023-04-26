/* -*- Mode: Prolog -*- */
:- module(auth_ajax,
	  [
	      request_ajax_id/2,
	      ajax_id_time_valid/1,
	      new_ajax_id/2
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_header)).

:- use_module(library(debug)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(db/redaktantoj)).

% Iel aŭtomata utf8 ne funkcias kiam redaktilo-test 
% lanĉiĝas kiel systemd-servo (per sistemestro "root")
% eble pro medio-variablo LC_LANG aŭ simile?
:- encoding(utf8).

:- multifile http:authenticate/3.

/** <module> auth_ajax
 * 
 * Tiu ĉi modulo ebligas aŭtentigi Ajax-petojn (JavaSkripto XMLHttpRequest).
 * Ĉar tiuj okazas fone de retpaĝo, oni ne povas peti uzanton ensaluti. Anstataŭe ĉe la komenca
 * saluto al la retpaĝo ni kreas kuketon, kiu entenas identigajn informojn pri la uzanto
 * kaj ĉe Ajax-petoj malpakas kaj kontrolas tiun kuketon.
 * La modulo eluzas la SWIPL-modulon https://www.swi-prolog.org/pldoc/man?section=httpauthenticate
 * kaj registras ĝin en ties kadro.
 * Poste la aŭtentigo-metodon vi povas registri por certa petfunkcio ekz-e tiel:
 * :- http_handler(red(revo_artikolo), revo_artikolo, [authentication(ajaxid)]).
*/


% registri la aŭtentigan predikaton ajax_auth kadre de httpauthenticate
http:authenticate(ajaxid,Request,[user(User),email(Email)]) :- ajax_auth(Request,User,Email).

%%/*** workaround bug in SWI 8.0.3 - exchanged Request / Request0 in append ***/
%%:- http_request_expansion(my_auth_expansion, 10). % 110?
%%my_auth_expansion(Request0, Request, Options) :-
%%	%debug(auth,'>> my_auth_exp ~q ~q',[Request0,Options]),
%%    http_dispatch:authentication(Options, Request0, Extra),
%%	append(Extra, Request0, Request).
%%	%debug(auth,'<< my_auth_exp ~q',[Request]).
%%/*****************/	

%! ajax_auth(+Request,-RedID,-Email)
%
% Kontroli ĉu la peto enhavas identigan kuketon, se jes ni ricevas
% la identigilon de la redaktanto kune kun ties registrita retadreso
% se la kuketo mankas, sed ni havas validan seancon, ni povas krei la
% kuketon resendonte ĝin al la retumilo por postaj petoj.
% Fine, se la kuketo ne plu validas, ni rifuzas la peton kaj
% postulas resaluton al la retpaĝo.
ajax_auth(Request,RedID,Email) :-
    %debug(redaktilo(ajaxid),'AjaxAuth...',[]),
	once((
        % ni ricevis kuketon AjaxID el redaktanto ID kaj kreo-tempo
		ajax_user(Request,RedID,ClientIP,Email),
		debug(redaktilo(ajaxid),'AjaxAuth: ~q ~q',[RedID,ClientIP])
		;
		% AjaxID mankas, sed se ni havas ankoraŭ seancon, ni kreu novan AjaxID el seanco+peto
		new_ajax_id_cookie(Request,RedID,Cookie),
		debug(redaktilo(ajaxid),'AjaxID: ~q',[Cookie]),
		format('Set-Cookie: ~w\r\n',[Cookie])
		; 
		% Ajax-hash ne (plu) valida, necesas resaluto
		format('Status: ~d~n~n',[401]),
		throw(http_reply(html([': Tro longa tempo pasis post saluto, necesas resaluti nun.\n'])))
    )). 


%! ajax_user(+Request,RedID,ClientIP,Retadreso) is det.
%
% Identigo per kuketo AjaxID, kiu konsistas el redaktanto ID kaj kuketo-kreo-tempo signitaj per HMAC.
% En la kalkuladon de HMAC eniras ankaŭ la klienta IP, tiel ke ĝi ne validas por alia kliento, se li
% ŝtelis ĝin
ajax_user(Request,RedID,ClientIP,Retadreso) :-
    %debug(redaktilo(ajaxid),'AjaxRequest ~q',[Request]),
    member(peer(ip(A,B,C,D)),Request),
    %debug(redaktilo(ajaxid),'AjaxID_a ~q',[Cookies]),
    atomic_list_concat([A,B,C,D],'.',ClientIP),
    %debug(redaktilo(ajaxid),'AjaxID_b ~q',[ClientIP]),
    request_ajax_id(Request,AjaxID),
    %debug(redaktilo(ajaxid),'AjaxID_c ~q ~q',[AjaxID,ClientIP]),
    sub_atom(AjaxID,0,20,_,RedID),
    sub_atom(AjaxID,20,8,_,HexTime),
    sub_atom(AjaxID,28,20,0,HexMac),
    %debug(redaktilo(ajaxid),'AjaxID_d ~q ~q ~q',[RedID,HexTime,HexMac]),
    % ĉu la kuketo estas ankoraŭ valida (laŭ tempo)?
    ajax_id_time_valid(AjaxID),
    % ĉu HMAC estas valida, t.e. kongrua kun la enhavo de la kuketo?
    ajax_hmac(RedID,ClientIP,HexTime,HexMac),
    % legu la retadreson per RedID el la redaktanto-db
	db_redaktantoj:email_redid(Retadreso,RedID).
    %debug(redaktilo(ajaxid),'AuthOK ~q',[Retadreso]).

ajax_id_time_valid(AjaxID) :-
    sub_atom(AjaxID,20,8,_,HexTime), 
    get_time(Now), hex_value(HexTime,Time),
    Valid is Time + 3 * 24 * 3600,
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
	     
%! new_ajax_id(+Request,-AjaxID) is det.
%
% Kreas novan identigilon por Ajax-petoj.

new_ajax_id(Request,AjaxID) :-
    new_ajax_id(Request,_,AjaxID,_).
	   

%! new_ajax_id(+Request,+RedId,-AjaxID,-Time) is det.
%
% Kreas novan identigilon el RedID, IP kaj tempo
% subskribitan per HMAC

new_ajax_id(Request,RedId,AjaxID,Time) :-
    % get client information
    %debug(redaktilo(ajaxid),'AjaxID_1 ~q',Request),
    member(peer(ip(A,B,C,D)),Request),
    %debug(redaktilo(ajaxid),'AjaxID_2 ~q ~q',[A,B]),
    atomic_list_concat([A,B,C,D],'.',ClientIP),
    http_in_session(_), % demandi tion unue evitas escepton je la sekva demando,
                        % anstataŭ ne revenas per 'fail.' kaj tiel povas redoni HTTP-401 en ajax_auth.
    http_session_data(retadreso(Retadreso)),
    db_redaktantoj:email_redid(Retadreso,RedId),
    %debug(redaktilo(ajaxid),'AjaxID_3 ~q ~q',[RedId,Retadreso]),
    % calculate hmac
    get_time(Time), Seconds is floor(Time),
    format(atom(HexTime),'~16r',[Seconds]),
    ajax_hmac(RedId,ClientIP,HexTime,HexMac20),
    atomic_list_concat([RedId,HexTime,HexMac20],'',AjaxID).
    %debug(redaktilo(ajaxid),'AjaxID_6 ~q ~q',[String,AjaxID]),
 
%! new_ajax_id_cookie(+Request,+RedID,-Cookie) is det.
%
% Kreas identigan kuketon el RedID, IP kaj tempo
new_ajax_id_cookie(Request,RedID,Cookie) :-
    new_ajax_id(Request,RedID,AjaxID,Time),	
    setting(http:prefix,Prefix),
    Time24 is Time + 3 * 24 * 3600,
    http_timestamp(Time24,Expires),
    format(atom(Cookie),'redaktilo_ajax=~w;Expires=~w;Path=~w; Version=1',
	   [AjaxID,Expires,Prefix]).

%! ajax_hmac(+RedId,+ClientIP,+HexTime,?HexMac20) is det.
%
% La predikato redonas True, se la unuaj 20 ciferoj de HMAC
% kongruas kun la kunaĵo de RedId, ClientIP kaj HexTime
% Krom kontroli, per la predikato vi ankaŭ povas kalkuli novan HMAC
% por subskribi la inform-triopon
ajax_hmac(RedId,ClientIP,HexTime,HexMac20) :-
    %debug(redaktilo(ajaxid),'AjaxID_hmac ~q ~q ~q',[RedId,ClientIP,HexTime]),
    atomic_list_concat([RedId,ClientIP,HexTime],String),
    agordo:get_config(ajax_secret,AjaxSecret),
    %debug(redaktilo(ajaxid),'AjaxID_hmac ~q',[String]),
    hmac_sha(AjaxSecret,String,HMac,[algorithm(sha256)]),
    hash_atom(HMac,Hex), 
    %debug(redaktilo(ajaxid),'AjaxID_hmac 3: ~q ~q',[Hex,HexMac20]),
    sub_atom(Hex,0,20,_,HexMac20).
    %debug(redaktilo(ajaxid),'AjaxID_hmac 4',[]).


