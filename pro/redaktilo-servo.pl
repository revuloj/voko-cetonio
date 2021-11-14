/* -*- Mode: Prolog -*- */
:- module(redaktilo_servo,
	  [ server/1			% +Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)). % reading post data
:- use_module(library(http/http_session)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_unix_daemon)).
%%:- use_module(library(http/http_openid)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).

:- use_module(library(settings)).
:- use_module(library(xpath)).

:- multifile http:location/3.
:- dynamic   http:location/3.

% TODO: is http/http_error autoloaded?
% see http://www.swi-prolog.org/pldoc/man?section=http-debug

:- use_module(library(debug)).

user:file_search_path(pro, './pro'). % aŭ: current_prolog_flag(home, Home). ...

% difinu la aplikaĵon "redaktilo"
/***
:- use_module(redaktilo:library(pengines)).
:- use_module(redaktilo:redaktilo).
***/
%:- use_module(redaktilo).
:- use_module(pro(auth/auth_page)).
:- use_module(pro(auth/auth_ajax)).
% legu post redaktilo_auth, por difini oauth2:server_attribute (multifile)
:- use_module(pro(cfg/agordo)).

:- use_module(sercho).
:- use_module(sendo).
:- use_module(gist).
:- use_module(pro(db/revo)).
:- use_module(pro(db/redaktantoj)).
:- use_module(xml_quote).
%:- use_module(xslt_trf).
:- use_module(xslt_proc).
%:- use_module(ekzfnt).
%%:- use_module(relaxng).

:- debug(redaktilo(_)).
:- debug(http(request)).
%% :- debug(openid(_)).
:- debug(sendo).

revo_url('https://reta-vortaro.de/revo/').

% aktuale la demono malŝaltas protokoladon per "debug"
% kaj ŝalto per komandlinio ne funkcias...
thread_init :-
    debug(redaktilo(_)),
    debug(http(request)),
    %% debug(openid(ax)), % yadis, authenticate, verify, resolve, check_authentication, crypt, associate...
    debug(sendo).

:- initialization(init).
:- initialization(help,main).
%%%%:- thread_initialization(thread_init).

init :-
    set_prolog_flag(encoding,utf8),
    agordo:read_auth_cfg,
    agordo:get_config([
     http_app_root(AppRoot),
	 http_app_scheme(Scheme),
	 http_app_host(Host),
	 http_app_port(Port),
	 http_session_timeout(Timeout)
	]),
    set_setting(http:prefix,AppRoot),
    set_setting(http:public_scheme,Scheme),
    set_setting(http:public_port,Port),
    set_setting(http:public_host,Host),
    http_set_session_options([
	  cookie(redaktilo_seanco),
    %%%  create(noauto),
      timeout(Timeout),
	  path(AppRoot)
	]),
    % la lokaj dosierujoj el kiuj servi dosierojn
    agordo:get_path(root_dir,web_dir,WebDir),
    agordo:get_path(root_dir,voko_dir,VokoDir),
    assert(user:file_search_path(web,WebDir)),
    assert(user:file_search_path(static,web(static))),
    assert(user:file_search_path(voko,VokoDir)),
    assert(user:file_search_path(cfg,voko(cfg))),
    assert(user:file_search_path(jsc,voko(jsc))),  
    assert(user:file_search_path(stl,voko(stl))),
    assert(user:file_search_path(smb,voko(smb))).
		  
http:location(red,root(red),[]).
http:location(static,root(static),[]).

/*** workaround bug in SWI 8.0.3 - exchanged Request / Request0 in append ***/
%% :- abolish(http_dispatch:request_expansion/2).
%% :- http_request_expansion(my_auth_expansion, 100).
%% my_auth_expansion(Request0, Request, Options) :-
%% 	%debug(auth,'>> my_auth_exp ~q ~q',[Request0,Options]),
%%     http_dispatch:authentication(Options, Request0, Extra),
%% 	append(Extra, Request0, Request).
%% 	%debug(auth,'<< my_auth_exp ~q',[Request]).

% redirect from / to /redaktilo/red, when behind a proxy, this is a task for the proxy
:- http_handler('/', http_redirect(moved,root(red)),[]).
:- http_handler(root(.), http_redirect(moved,root('red/')),[]).

% uzas padon web...
%%:- http_handler(red(.), reply_files, [prefix, authentication(local), authentication(oauth), id(landing)]).
:- http_handler(red(.), reply_files, [prefix, authentication(page), id(landing)]).
%% provizore provu sen saltuto...
%%:- http_handler(red(.), reply_files, [prefix,id(landing)]).

 % uzas padon static ...
:- http_handler(static(.), reply_static_files, [prefix]).

:- http_handler(red(revo_preflng), revo_preflng, [authentication(ajaxid)]).
:- http_handler(red(revo_artikolo), revo_artikolo, [authentication(ajaxid)]).
:- http_handler(red(revo_lastaj_redaktoj), revo_lastaj_redaktoj, [authentication(ajaxid)]).
:- http_handler(red(revo_sendo), revo_sendo, [authentication(ajaxid)]).
:- http_handler(red(revo_sercho), revo_sercho, [authentication(ajaxid)]).
:- http_handler(red(revo_kontrolo), revo_kontrolo, [authentication(ajaxid)]).
:- http_handler(red(revo_rigardo), revo_rigardo, [authentication(ajaxid)]).
%:- http_handler(red(revo_bibliogr), revo_bibliogr, []).
:- http_handler(red(citajho_sercho), citajho_sercho, [authentication(ajaxid)]).
:- http_handler(red(bildo_sercho), bildo_sercho, [authentication(ajaxid)]).
:- http_handler(red(bildo_info), bildo_info, [authentication(ajaxid)]).
:- http_handler(red(bildeto_info), bildeto_info, [authentication(ajaxid)]).
:- http_handler(red(bildo_info_2), bildo_info_2, [authentication(ajaxid)]).
:- http_handler(red(analizo), analizo, [authentication(ajaxid)]).
:- http_handler(red(analinioj), analinioj, [authentication(ajaxid)]).
:- http_handler(red(homonimoj_senref), homonimoj_senref, [authentication(ajaxid)]).

:- http_handler(root(voko), serve_files_in_directory(cfg), [prefix]).
:- http_handler(root(stl), serve_files_in_directory(stl), [prefix]).
:- http_handler(root(smb), serve_files_in_directory(smb), [prefix]).
:- http_handler(root(jsc), serve_files_in_directory(jsc), [prefix]).
:- http_handler(root(bld), proxy_bld, [prefix]).

server(Port) :-
    show_pathes,
    show_handlers,
    http_server(http_dispatch, [port(Port)]).

daemon :-
    show_pathes,
    show_handlers,
    http_daemon.

help :-
    format('~`=t~51|~n'), 
    format('|               Revo-Redaktilo.~t~50||~n'),
    format('~`=t~51|~n~n'),
    format('Programo por lanĉi la Revo-redaktilon. Vi povas aŭ~n'),
    format('tajpi interage ĉe la prolog-interpretilo: ~n~n'),
    format('   server(8080) ~n~n'),
    format('por lanĉi la servon ĉe retpordo 8080;~n'),
    format('aŭ lanĉi ĝin kiel fona servo,~n'),
    format('t.e. demono, per la predikato "daemon".~n'),
    format('Vidu la tiucelan skripton "run-server.sh".~n~n'),
    prolog.
	       

/*******************************************/

show_pathes :-
    forall(
        (
            member(Path,[web,voko,static,stl,cfg,smb,js,jsc,icons,css]),
            file_search_path(Path,Dir)
        ),
        %debug(redaktilo(pado),'~q -> ~q',[Path,Dir])
        format('~q -> ~q~n',[Path,Dir])
    ). 

show_handlers :-
    http_dispatch:path_tree(T),
    forall(
        %http:location(P,H,_),
        member(node(P,H,_,_),T),
        format('~q >> ~q~n',[P,H])
    ).


entry_no_cache(Request) :-
    member(path(Path),Request),
    sub_atom(Path,_,1,0,'/'),
    writeln('Cache-Control: no-cache, no-store,  must-revalidate'),
    writeln('Pragma: no-cache'),
    writeln('Expires: 0'), !.

entry_no_cache(_).

reply_files(Request) :-
    % evitu reveni al saluto-paĝo ĉiam denove
%%%    entry_no_cache(Request),
   
    debug(redaktilo(request),'handler reply_files',[]),
    http_reply_from_files(web(.), [indexes(['redaktilo.html'])], Request),
    debug(redaktilo(request),'<< reply_files',[]).


reply_static_files(Request) :-
    % ne protektitaj publikaj dosieroj
    debug(redaktilo(request),'handler reply_static_files',[]),
    http_reply_from_files(static(.), [indexes(['notoj-pri-versio.html'])], Request).

% preferataj lingvoj
revo_preflng(Request) :-   
        once((
        member(accept_language(AccLng),Request),
        preferataj_lingvoj(AccLng,Lingvoj)
        ;
        Lingvoj =''
	)),
    reply_json(Lingvoj).

% elŝutas artikolon el retavortaro.de kaj sendas al la krozilo
revo_artikolo(Request) :-   
    debug(redaktilo(request),'~q',[Request]),
    % get art parameter
    http_parameters(Request,
		   [art(Art, [length>0,length<50])]),
    debug(redaktilo(request),'art=~q',[Art]),
    
    % check data/input parameters...
    check_param_all(Art,csym),
    check_param_all(Art,ascii),

    % legu artikolon
    xml_stream(Art,XmlStream,Status),
    (Status = 200
     ->
        set_stream(XmlStream,encoding(utf8)),
        set_stream(current_output,encoding(utf8)),
        active_sessions_header,
        format('Content-type: text/plain; charset=UTF-8~n~n'),
        %copy_stream_data(XmlStream,current_output),
        unquote_stream(XmlStream,current_output),
        close(XmlStream)
      ;
        format('Status: ~d~n~n',[Status])
     ).

xml_stream(FileName,XmlStream,Status) :-
    revo_url(Revo),
    % antaŭŝargu DTD
    %%  vokodtd(vortaro,VokoDTD),
    % elŝutu XML
    atomic_list_concat([Revo,'xml/',FileName,'.xml'],Url),
    http_open(Url,XmlStream,[status_code(Status)]),!.


unquote_stream(InStream,OutStream) :-
    repeat,
        read_line_to_codes(InStream,Line),
        ( Line == end_of_file
        -> !
        ;
        xml_unquote_cdata(Line,ULine),
        format(OutStream,'~s~n',[ULine]),
        fail
        ).           

% serĉas en la sql-datumbazo laŭ kapvorto-komenco kaj redonas
% informojn (marko, senco k.s. wn formo JSON 
revo_sercho(Request) :-
(
	debug(redaktilo(request),'~q',[Request]),
	% read post data
	% member(method(post), Request), !,
	http_parameters(Request,
	    [
			lng(Lng,[ oneof([eo]) ]),
			sercho(Sercho,[length>0,length<100])
	    ]),
	debug(redaktilo(request),'lng=~q, sercho=~q',[Lng,Sercho]),

	% validate params
	Lng=eo,
	check_param_none(Sercho,cntrl),
	check_param_none(Sercho,quote),
	check_param_none(Sercho,punct,[0'-]),
	
    set_stream(current_output,encoding(utf8)),
    active_sessions_header,    
	
	% format('Content-type: application/json~n~n'),
	findall(Json,search_eo_json(Sercho,Json),ResultList)
	       % debug(redaktilo(request),'json=~q',[ResultList]),
	       % json_write(current_output,ResultList).
        -> reply_json(ResultList)
        ;
	format('Status: ~d~n~n',[400])
).


revo_lastaj_redaktoj(Request) :-   
    once((
        member(user(RedID),Request),
        email_redid(Retadreso,RedID)
        ;
        http_session_data(retadreso(Retadreso))
    )),
    % respondu kaj sendu
    active_sessions_header,
    get_gists(Retadreso).


% forsendas artikolon senditan de la krozilo per afiŝado ĉe gist.github.com
revo_sendo(Request) :-   
    debug(redaktilo(request),'revo_sendo: ~q',[Request]),
    % read parameters
    http_parameters(Request,
		[
			shangho(Shangho_au_Nomo, [length>1,length<500]),
			redakto(Redakto, [oneof([redakto,aldono]),default(redakto)]),
            dosiero(Dosiero, [length>0,length<50]),
			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
        ]),
    % http_session_data(retadreso(Retadreso)),
    once((
        member(user(RedID),Request),
        email_redid(Retadreso,RedID)
        ;
        http_session_data(retadreso(Retadreso))
    )),

%    sqlrevo:email_redid(Retadreso,RedID),
    debug(redaktilo(request),'shangho=~q',[Shangho_au_Nomo]),

    % kodigu specialajn literojn ktp. per unuoj
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    atom_codes(Xml,Codes),
    xml_quote_cdata(Codes,Quoted,ReverseEntInx,EntVal1Inx,utf8),

    % respondu kaj sendu
    active_sessions_header,
    %format('Content-type: application/json~n~n'),

    post_gist(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Quoted).
    
% forsendas artikolon senditan de la krozilo per retpoŝto
%revo_sendo(Request) :-    
%    debug(redaktilo(request),'~q',[Request]),
%    % read post data
%    http_parameters(Request,
%		[
%			shangho(Shangho_au_Nomo, [length>1,length<500]),
%			redakto(Redakto, [oneof([redakto,aldono]),default(redakto)]),
%			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
%		]),
%    debug(redaktilo(request),'shangho=~q',[Shangho_au_Nomo]),
%
%     % http_session_data(retadreso(Retadreso)),
%    member(email(Retadreso),Request),
%    %sqlrevo:email_redid(Retadreso,RedID),
%
%    % kodigu specialajn literojn ktp. per unuoj
%    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
%    atom_codes(Xml,Codes),
%    xml_quote_cdata(Codes,Quoted,ReverseEntInx,EntVal1Inx,utf8),
%
%    % respondu kaj sendu
%    active_sessions_header,
%    format('Content-type: text/html~n~n'),
%    % FIXME: pli bone havu apartan funkcion por sendi novan artikolon?
%    once((
%        Redakto = redakto,
%        send_revo_redakto(Retadreso,Shangho_au_Nomo,Quoted)
%        ;
%        Redakto = aldono,
%        send_revo_aldono(Retadreso,Shangho_au_Nomo,Quoted)
%        ;
%        %format('Status: ~d~n~n',[401]),
%        throw(http_reply(html(['Neatendita eraro dum forsendo.\n'])))
%    )),
%    format('Bone. Sendita.').


% sintaks-kontrolo de la artikolo per Jing
revo_kontrolo(Request) :-
    debug(redaktilo(request),'~q',[Request]),
    % read post data
    http_parameters(Request,
		[
			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
		]),
%% KOREKTU: necesas voki kontrol-servon nun anstataŭ rekte Javon/Jing
    %relaxng_json(Xml,Json),
    %reply_json(Json).
    agordo:get_config(http_rng_url,Url),
    %uri_components(Url,uri_components(Scheme,Auth,Path,_,_)),
    %uri_components(Url1,uri_components(Scheme,Auth,Path,_,_)),
    debug(redaktilo(kontrolo),'url ~q',[Url]),
    http_open(Url,Stream,[header(content_type,_ContentType),post(atom(Xml))]),
    %format('Content-type: ~w~n~n',[ContentType]),
    active_sessions_header,
    format('Content-type: application/json; charset=UTF-8~n~n'), % alternative sendu kiel teksto la la retumilo
            % kaj traktu tie per Javoskripto...
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    write('['),
    %copy_stream_data(Stream,current_output),
    err_text_json(Stream,current_output),
    write(']'),
    close(Stream).

err_text_json(In,Out) :-
    repeat,
    read_line_to_codes(In, Codes),
    (   Codes == end_of_file
    ->  !
    ;   atom_codes(Line, Codes),
        err_line_json(Line,Json),
        json_write(Out,Json),
        fail
    ).

err_line_json(Line,_{line: L, pos: Pos, msg: Msg}) :-
    atomic_list_concat([_,L,P|Rest],':',Line),
    once((
        atom_number(P,_), Pos=P, atomic_list_concat(Rest,':',Msg)
        ;
        Pos=0, atomic_list_concat([P|Rest],':',Msg)
    )).

% HTML-antaurigardo de la artikolo
revo_rigardo(Request) :-   
    debug(redaktilo(request),'~q',[Request]),
    % read parameters
    http_parameters(Request,
		[
			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
		]),
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    atom_codes(Xml,Codes),
    xml_quote_cdata(Codes,Quoted,ReverseEntInx,EntVal1Inx,utf8),

    % Quoted -> HTML
    catch(
	    (
            agordo:get_path(root_dir,voko_xsl,VokoXsl),
            %sub_atom(VokoXslUri,7,_,0,VokoXsl),
            set_stream(current_output,encoding(utf8)),
            xslt_proc(VokoXsl,Quoted,Html),
            active_sessions_header,
            format('Content-type: text/html~n~n'),
            format('~s',[Html])
        ),
      xslt_exception(Exc,Msg), 
        (
            debug(redaktilo(rigardo),'~q: ~q',[Exc,Msg]),
            format('Status: ~d~n~n',[400]),
            set_stream(current_output,encoding(utf8)),
            write(Exc),
            write(Msg)
        )
    ).

citajho_sercho(Request) :-
    http_parameters(Request,
    [
	    sercho(Sercho, [length>1,length<500]),
	    kie(Kie, [oneof([vikipedio,anaso,klasikaj,postaj])]) 
    ]),
    sercho(Kie,Sercho).

bildo_sercho(Request) :-       
    http_parameters(Request,
    [
        sercho(Sercho, [length>1,length<500]),
        kie(_, [oneof([vikimedio])]) 
    ]),
    debug(sercho(what),'<<< VIKIMEDIO: ~w',[Sercho]),
    active_sessions_header,
    format('Content-type: application/json~n~n'),
    sercho:bildo_sercho(Sercho,_).

bildo_info(Request) :-
    http_parameters(Request,
    [
        paghoj(Paghoj, [length>1,length<500]),
        kie(_, [oneof([vikimedio])]) 
    ]),
    debug(sercho(what),'<<< VIKIMEDIO: ~w',[Paghoj]),
    format('Content-type: application/json~n~n'),
    sercho:bildo_info(Paghoj).

bildo_info_2(Request) :-
    http_parameters(Request,
    [
        dosiero(Dosiero, [length>1,length<500]),
        kie(_, [oneof([vikimedio])]) 
    ]),
    debug(sercho(what),'<<< VIKIMEDIO: ~w',[Dosiero]),
    format('Content-type: application/json~n~n'),
    sercho:bildo_info_2(Dosiero).

bildeto_info(Request) :-
    http_parameters(Request,
    [
        dosieroj(Dosieroj, [length>1,length<5000]),
        kie(_, [oneof([vikimedio])]) 
    ]),
    debug(sercho(what),'<<< VIKIMEDIO: ~w',[Dosieroj]),
    format('Content-type: application/json~n~n'),
    sercho:bildeto_info(Dosieroj).    

analinioj(Request) :-
    debug(redaktilo(analinioj),'ANA req ~q',[Request]),
    http_read_json(Request, JSON, [json_object(dict)]),
    debug(redaktilo(analinioj),'ANA json ~q',[JSON]),

    agordo:get_config(http_ana_url,Url),
    uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
    atom_concat(Root,'/analinioj',Path),
    uri_components(Url1,uri_components(Scheme,Auth,Path,'',_)),
    debug(redaktilo(analinioj),'url ~q',[Url1]),

    http_post(Url1, json(JSON), Reply, []),
    debug(redaktilo(analinioj),'ANA reply ~q',[Reply]),

    % la rezultojn el la proxy-konekto plusendu al la retumilo
    reply_json(Reply).


analizo(Request) :-
    http_parameters(Request,
    [
        teksto(Teksto, [length<150000])
    ]),

    agordo:get_config(http_ana_url,Url),
    uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
    atom_concat(Root,'/analizo',Path),
    uri_query_components(Query,[teksto(Teksto)]),
    uri_components(Url1,uri_components(Scheme,Auth,Path,Query,_)),
    debug(redaktilo(analizo),'url ~q',[Url]),
    http_open(Url1,Stream,[header(content_type,ContentType)]),

    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).


homonimoj_senref(_Request) :-
    set_stream(current_output,encoding(utf8)),
	
    % format('Content-type: application/json~n~n'),
    findall(Json,homonimoj_sen_ref_json(Json),ResultList)
    -> reply_json(ResultList)
    ;
    format('Status: ~d~n~n',[500]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_param_all(Param,Type) :-
    atom_codes(Param,Codes),
    check_codes_all(Codes,Type).

check_param_none(Param,Type) :-
    atom_codes(Param,Codes),
    check_codes_none(Codes,Type).

check_param_none(Param,Type,Permitted) :-
    atom_codes(Param,Codes),
    check_codes_none(Codes,Type,Permitted).

check_codes_all([],_).
check_codes_all([C|Codes],Type) :-
    code_type(C,Type),
    check_codes_all(Codes,Type).

check_codes_none([],_).
check_codes_none([C|Codes],Type) :-
    code_type(C,Type) -> false
    ; check_codes_none(Codes,Type).

check_codes_none([],_,_).
check_codes_none([C|Codes],Type,Permitted) :-
    code_type(C,Type), \+ memberchk(C,Permitted) -> false
    ; check_codes_none(Codes,Type,Permitted).

sub_list(List,Start,Len,SubList) :-
    S_1 is Start-1,
    length(L,S_1),
    append(L,R,List),
    length(SubList,Len),
    append(SubList,_,R).

% ekz. 	'de-DE,de;q=0.8,en-US;q=0.6,en;q=0.4,eo;q=0.2' -> [de,en]
%       'de-DE,en-GB' -> [de,en]
%       'de-DE' -> [de]
preferataj_lingvoj(AccLng,Lingvoj) :-
    atomic_list_concat(LngLst,';',AccLng),
    setof(Lingvo,
	  L^L1^Lng^Country^(
	      member(L,LngLst),
	      atomic_list_concat(L1,',',L),
	      member(Lng,L1),
	      once((
		  atom_length(Lng,2), Lingvo = Lng
	          ;
		  atomic_list_concat([Lingvo,Country],'-',Lng)
	      )),
	      Lingvo \= eo
	  ), Lingvoj).

% kiom da seancoj (uzantoj) momente aktivas,
% tio povas esti erariga, ĉar
% - se oni plurfoje salutas sen malsaluti oni kalkuliĝas dum iom da tempo plurfoje
% - kiam la seanco finiĝis, ajax_id povas esti daŭre funkcianta

active_sessions_header :-
        bagof(S,
            Idle^(
                http_current_session(S,idle(Idle)),Idle<3600
            ),Sessions),
        length(Sessions,Cnt),!,
        format('Revo-Seancoj: ~d~n',Cnt).
    
    % kaze ke ne ekzistas seancoj, evitu malsukcesi per false...
    active_sessions_header :- true.
    