/* -*- Mode: Prolog -*- */
:- module(auth_page,[]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)). % reading post data
:- use_module(library(http/http_session)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_host)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(settings)).

:- use_module(pro(auth/oauth2)).
:- use_module(pro(auth/server_auth)).

:- multifile http:location/3, user:body//2.
:- dynamic   http:location/3.

:- use_module(library(debug)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(db/redaktantoj)).
:- use_module(pro(param_checks)).

% iel aŭtomata utf8 ne funkcias kiam redaktilo-test 
% lanĉiĝas kiel systemd servo (per sistemestro "root")
% eble pro LC_LANG medio-variablo aŭ simile?
:- encoding(utf8).

:- multifile http:authenticate/3.
http:authenticate(oauth,Request,[user(User)]) :- page_auth(Request,User).

http:location(auth, root(auth), []).
http:location(reg,root(reg),[]).

% saltuo-paĝoj
:- http_handler(auth(login_page), revo_login_page, [id(login_page)]).
%:- http_handler(root(redaktilo_saluto), redaktilo_saluto, []).

% registro-paĝoj
:- http_handler(reg(revo_registro), revo_registro, [id(register_page)]).
:- http_handler(reg(revo_registro1), revo_registro1, []).

user:body(saluto, Body) -->  html(body([onload='checkCookieConsent()'],Body)).


/************* Saluto kaj registrado ********************/

revo_login_page(_Request) :-
	debug(auth, 'revo_login_page >>',[]),
    
    reply_html_page(saluto,
		    [ title('Saluto al Redaktilo'),
%		      link([rel="stylesheet",type="text/css",href="../css/openid.css"],[]),
		      link([rel="stylesheet",type="text/css",href="../static/static-pages.css"],[]),
		      script([src="../static/kuketoj.js"],[]),
		      script([src="https://code.jquery.com/jquery-3.2.1.js"],[])
		    ],
			[\redaktilo_saluto([return_to([])])]),
	debug(auth, '<< revo_login_page',[]).

    
revo_registro(_Request) :-
	once((
		http_in_session(_),
		http_session_data(oauth2id(SubId)),
		debug(redaktilo(auth),'registro, oauth2id: ~q',[SubId]),
		string(SubId),
		reply_html_page([ title('Registrado de via konto')
		    ],
			[ \register_email_form(SubId,[]) ])
		;
		throw(http_reply(forbidden(location_by_id(register_page))))
	)).

revo_registro1(Request) :-
    http_parameters(Request,
		    [
			retposhto(Retadreso,[length>0,length<100])
			]),
	(\+ http_in_session(_) 
		-> throw(http_reply(forbidden(location_by_id(register_page))))
		; true),

	once((		
		% ni uzas la retadreson, kiun donis la uzanto, kiu povas
		% devii de tiu ĉe la salut-provizanto, sed devas esti registrata
		% en la listo de redaktantoj

		check_email(Retadreso,RetadresoChecked),
		% http_session_data(oauth2id(SubId)),
		user_info(Provider,UserInfo),

		debug(auth,'registro1, user_info: ~q',[UserInfo]),
		user_info_sub(Provider,UserInfo,SubId),

		% registri SubId por la al ni konata redaktanto
		% kaj memoru lian RedId kaj preferatan retpoŝton en la seanco
		editor_update_subid(RetadresoChecked,Provider,SubId),
		editor_by_subid(SubId,Provider,row(RedID,_,RetadresoPreferata)),
		session_assert_user(RedID,RetadresoPreferata),			

		http_redirect(moved_temporary,location_by_id(landing),_)
		;
		
        % FARENDA: ĉu funkciis jam se la redaktanto havas plurajn retadresojn...?
        % Se la retadreso ne funkcias, avertu la uzanton...
		format('Content-type: text/html; charset=UTF-8~n'),
		format('Status: ~d~n~n',[302]),
		set_stream(current_output,encoding(utf8)),
		write("<html><body><h1>Retadreso ne valida!</h1>"),
		write("<p>Pardonu, tio ne funkciis: via retadreso ne estas valida aŭ ne jam registrita kiel redakta redadreso. "),
		write("Bv. turniĝi al la administrantoj de Reta Vortaro."),
		write("</body></html>")
    )).	    

register_email_form(SubId,Options) -->
    {
	http_link_to_id(revo_registro1, [], Registro), % /redaktilo/reg/revo_registro1
	option(action(Action), Options, Registro)
    },
    html(div([],
		 [ %\openid_title,
		   form([ name(register),
			  id(register),
			  action(Action),
			  method('GET')
			],
			[
			    h1(['Registrado de via retpoŝtadreso']),
			    div(
			      [style='background-color: #f5f3e5; padding: 2em'],
			      [
			       p(['Vi sukcese salutis.']),
			       p(['Necesas ligi vian konton kun la retpoŝtadreso uzata de vi kiel redaktanto. ',
				  'Bonvolu doni retpoŝtadreson per kiu vi registriĝis ĉe Reta Vortaro.'
				 ]),
			       p([
					span('Via subskribanto-numero:'), br(''),
	     				input([ 
							name(openid_url),
							id(openid_url),
							value(SubId),
							readonly(readonly),
							size(50)
					      ])
				    ]),
			       p([
					span('Via repoŝtadreso por redaktado:'), br(''),
					input([ 
						name(retposhto),
						id(retposhto),
						size(50),
						placeholder('Via retpoŝtadreso')
					      ])
				    ]),
			       input([ type(submit),
				       value('Registru!')
				     ])
				   ])
			])
		 ])).


redaktilo_saluto(_Options) -->	
    html(div([],
	     [
		  h2('Saluto al Redaktilo'),

	      p(['Per ensalutado al Redaktilo tra via konto ĉe unu el la malspuraj retprovizantoj vi konsentas uzi ',
			  'viajn personajn informojn kiel priskribita detale en la ',
		  a([href="../static/datumprotekto.html"],['Datumprotekta Deklaro']),'.']),

	      form([name="google",id="google",action="login",method="GET"],
 		    [input([type="hidden",name="server",value="google"]),
		     div([style="background-image: linear-gradient(90deg,#f5f3e5,transparent); padding: 1em; margin-bottom: 1em"],
		       [ %p(['Se vi havas konton ĉe Google:']),
		        input([type="image",id="google_login",src="../static/btn_google_signin_light_normal_web.png",disabled=disabled,
			       alt="Saluto per Google",title="Salutu per Google-konto",value="Google",style="margin-left:1em"])])]),

		 form([name="facebook",id="facebook",action="login",method="GET"],
			[input([type="hidden",name="server",value="facebook"]),
			div([style="background-image: linear-gradient(90deg,#f5f3e5,transparent); padding: 1em; margin-bottom: 1em"],
				[ %p(['Se vi havas konton ĉe Facebook:']),
				input([type="image",id="facebook_login",src="../static/btn_facebook_signin.png",disabled=disabled,
					alt="Saluto per Facebook",title="Salutu per Facebook-konto",value="Facebook",style="margin-left:1em"])])]),

	      form([name="yahoo",id="yahoo",action="login",method="GET"],		    
		   [input([type="hidden",name="server",value="yahoo"]),
		    div([style="background-image: linear-gradient(90deg,#f5f3e5,transparent); padding: 1em"],
			[ %p(['Se vi havas konton ĉe Yahoo:']),
			 input([type="image",id="yahoo_login",src="../static/btn_yahoo_signin.png",onclick="javascript:{$('form#login').submit();}",disabled=disabled,
				alt="Saluto per Yahoo",title="Salutu per Yahoo-konto",style="margin-left:1em"],[])
			])]),
	      
	      div([p(['Mi rekomendas informiĝi per ',a([href="../static/notoj-pri-versio.html"],['Notoj kaj konsiloj']),
		     ' pri uzado de la redaktilo kaj konataj eraroj.']),
		   p(['Aktuale estas jenaj antaŭkondiĉoj por povi saluti al la redaktilo:',
		      ul([
				li('Vi devas esti antaŭregistrita redaktanto ĉe Reta Vortaro.'),
			    li('Vi havas konton ĉe unu el la supraj provizantoj, permesanta saluti.')
			  ])
			])
		  ]),
	      div([class='kuketoaverto',id='kuketoaverto'],
			  [table([tr([
				  td([p(['Ni uzas kuketojn (retumilajn memoretojn). Uzante nian servon vi konsentas al konservado de informoj en kuketoj ',
				  'kaj uzo de via retadreso kaj nomo por identigi vin kaj marki viajn redaktojn. ',
				  'Eksciu pli pri la uzado de personaj datumoj en la ',
				  a([href='../static/datumprotekto.html'],['datumprotekta deklaro']),'.',br(''),br(''),
						  button([name='konfirmo',onClick='setCookieConsent()'],['Mi konfirmas'])
				 ])]),
				 td([img([src='../static/kaefer2.png'])])
				])])]
		)]) 
	 ).


/************* Kontrolu, ĉu uzanto jam salutis kaj rajtas uzi la paĝon ********************/

% Kontrolu, ĉu la uzanto jam salutis tra Google, FB, Yahoo... (OAuth2)
% kaj se ne, konduku lin al la saluto-paĝo

page_auth(_Request,RedID) :-
	debug(auth,'>> page_auth',[]),	
    once((
		oauth2_id(RedID)
		;
		http_redirect(moved_temporary, location_by_id(login_page), _)
    )).


oauth2_id(RedID) :-
	% ni havas seancon
	http_in_session(_SessionID),

	% salutinta per OpenId Connect (OAuth2)
    http_session_data(oauth2id(_UserOAuthId)),

	% ni havas jam redaktanton-identigilon kaj retadreson en la seanco, 
	% ne nepre la saman, kiun posedas la provizanto!
	session_data_user(RedID,_Retadreso).


auth_config:reply_logged_in(Options) :-
	% malpaku la informojn akiritajn dum la saluto
	debug(auth,'reply_logged_in, opts: ~q',[Options]),
	option(identity_provider(Provider), Options),
	option(user_info(UserInfo), Options),
	user_info_sub(Provider,UserInfo,SubId),
    http_session_assert(oauth2id(SubId)),
		  
    % trovu uzanton per subid aŭ email,
    % se ambau ne funkcias necesas registri lin/ŝin (per create_user)		  
    once((
		% ni rekonas la uzanton per lia SubId
		editor_by_subid(SubId,Provider,row(RedID,_,Retadreso)),
		session_assert_user(RedID,Retadreso),		
		http_redirect(moved_temporary, location_by_id(landing), _)		
		;

		% ni ne rekonas lin, ĉar li unuafoje salutis per tiu provizanto,
		% sed la provizanto donis al ni jam registritan retpoŝtadrson
		is_dict(UserInfo),
		% se ne trovita, ĉu ni ricevis konatan retadreson per Claim?
		UserInfo.get(email_verified) = 'true',
		% option(email(Email), Options),
		Email = UserInfo.get(email),
		editor_by_email(Email,row(RedID,_,Retadreso)), % preferata retadreso povas devii de Claim.email
		session_assert_user(RedID,Retadreso),	

		% memoru tiun SubId
		debug(auth,'konata retposhto, aktualigi subid: ~q',[SubId]),		
		editor_update_subid(Retadreso,Provider,SubId),
		http_redirect(moved_temporary, location_by_id(landing), _)
		;

		% se ankoraŭ ne trovita demandu la retadreson per registro-paĝo
		revo_registro(_)
    )).


user_info(Provider,UserInfo) :- 
	(var(Provider) ; Provider \= facebook), !,
	auth_config:user_info(_, Provider, UserInfo).

user_info(facebook,UserInfo) :- 
	http_in_session(_SessionID),
	http_session_data(oauth2(facebook, TokenInfo)),
	UserInfo = TokenInfo.get(user_info).

user_info_sub(facebook, UserInfo, UserInfo.id) :- !.
user_info_sub(_, UserInfo, UserInfo.sub).

auth_config:reply_logged_out(_Options) :-
	catch(http_session_retractall(oauth2id(_)), _, true),
	catch(http_session_retractall(red_id(_)), _, true),
  	catch(http_session_retractall(retadreso(_)), _, true),
	revo_login_page(_).

session_assert_user(RedID,Retadreso) :-
	http_session_assert(retadreso(Retadreso)),
	http_session_assert(red_id(RedID)).

session_data_user(RedID,Retadreso) :-
	http_session_data(retadreso(Retadreso)),
	http_session_data(red_id(RedID)).



/******************************* helpopredikatoj **********************/

%% entry_no_cache(_Request) :-
%% %  http_clean_location_cache,
%% %  member(path(Path),Request),
%% %  sub_atom(Path,_,1,0,'/'),
%%   writeln('Cache-Control: no-cache, no-store,  must-revalidate'),
%%   writeln('Pragma: no-cache'),
%%   writeln('Expires: 0'), !.

%% return_to_url(_Request,Url) :-
%% 	agordo:get_config([
%% 		http_app_root(AppRoot),
%% 		http_app_scheme(Scheme),
%% 		http_app_host(Host),
%% 		http_app_port(Port)
%% 	 ]),
%%     (scheme_port(Scheme,Port)
%%     -> format(atom(Url),'~w://~w~w',[Scheme,Host,AppRoot])
%%     ; format(atom(Url),'~w://~w:~w~w',[Scheme,Host,Port,AppRoot])).
%% 
%% scheme_port(http,80).
%% scheme_port(https,443).


