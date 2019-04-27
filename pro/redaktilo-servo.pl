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
:- use_module(library(http/http_openid)).
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

% difinu la aplikaĵon "redaktilo"
/***
:- use_module(redaktilo:library(pengines)).
:- use_module(redaktilo:redaktilo).
***/
:- use_module(agordo).
%:- use_module(redaktilo).
:- use_module(redaktilo_auth).
:- use_module(sendo).
:- use_module(sqlrevo).
:- use_module(xml_quote).
%:- use_module(xslt_trf).
:- use_module(xslt_proc).
%:- use_module(ekzfnt).
%%:- use_module(relaxng).

:- debug(redaktilo(_)).
:- debug(http(request)).
:- debug(openid(_)).
:- debug(sendo).

revo_url('http://retavortaro.de/revo/').

% aktuale la demono malŝaltas protokoladon per "debug"
% kaj ŝalto per komandlinio ne funkcias...
thread_init :-
    debug(redaktilo(_)),
    debug(http(request)),
    debug(openid(ax)), % yadis, authenticate, verify, resolve, check_authentication, crypt, associate...
    debug(sendo).

:- initialization(init).
:- initialization(help,main).
%%%%:- thread_initialization(thread_init).

init :-
    set_prolog_flag(encoding,utf8),
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
    assert(user:file_search_path(stl,voko(stl))),
    assert(user:file_search_path(smb,voko(smb))).
		  
http:location(red,root(red),[]).
http:location(static,root(static),[]).

% redirect from / to /redaktilo/red, when behind a proxy, this is a task for the proxy
:- http_handler('/', http_redirect(moved,root(red)),[]).
:- http_handler(root(.), http_redirect(moved,root('red/')),[]).
:- http_handler(red(.), reply_files, [prefix, authentication(openid)]).
:- http_handler(static(.), reply_static_files, [prefix]).

:- http_handler(red(revo_preflng), revo_preflng, [authentication(ajaxid)]).
:- http_handler(red(revo_artikolo), revo_artikolo, [authentication(ajaxid)]).
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

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

daemon :-
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

entry_no_cache(Request) :-
  member(path(Path),Request),
  sub_atom(Path,_,1,0,'/'),
  writeln('Cache-Control: no-cache, no-store,  must-revalidate'),
  writeln('Pragma: no-cache'),
  writeln('Expires: 0'), !.

entry_no_cache(_).

reply_files(Request) :-
    % evitu reveni al saluto-paĝo ĉiam denove
%%%   %%% entry_no_cache(Request),

%%    page_auth(Request),
    
    debug(redaktilo(request),'handler reply_files',[]),
    http_reply_from_files(web(.), [indexes(['redaktilo.html'])], Request).

reply_static_files(Request) :-
    % ne protektitaj publikaj dosieroj
    debug(redaktilo(request),'handler reply_static_files',[]),
    http_reply_from_files(static(.), [indexes(['notoj-pri-versio.html'])], Request).

% preferataj lingvoj
revo_preflng(Request) :-
%%    ajax_auth(Request),
    
    once((
	member(accept_language(AccLng),Request),
	preferataj_lingvoj(AccLng,Lingvoj)
	;
	Lingvoj =''
	)),

    reply_json(Lingvoj).

% elŝutas artikolon el retavortaro.de kaj sendas al la krozilo
revo_artikolo(Request) :-
%%    ajax_auth(Request),
    
    debug(redaktilo(request),'~q',[Request]),
    % read post data
    % member(method(post), Request), !,
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
%%    ajax_auth(Request),

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
	
	% format('Content-type: application/json~n~n'),
	findall(Json,sqlrevo:search_eo_json(Sercho,Json),ResultList)
	       % debug(redaktilo(request),'json=~q',[ResultList]),
	       % json_write(current_output,ResultList).
        -> reply_json(ResultList)
        ;
	format('Status: ~d~n~n',[400])
    ).


% forsendas artikolon senditan de la krozilo per retpoŝto
revo_sendo(Request) :-
%%    ajax_auth(Request),
    
    debug(redaktilo(request),'~q',[Request]),
    % read post data
    http_parameters(Request,
		    [
			shangho(Shangho_au_Nomo, [length>1,length<500]),
			redakto(Redakto, [oneof([redakto,aldono]),default(redakto)]),
			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
		    ]),
    http_session_data(retadreso(Retadreso)),
    debug(redaktilo(request),'shangho=~q',[Shangho_au_Nomo]),

    % kodigu specialajn literojn ktp. per unuoj
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    atom_codes(Xml,Codes),
    xml_quote_cdata(Codes,Quoted,ReverseEntInx,EntVal1Inx,utf8),

    % respondu kaj sendu
    format('Content-type: text/html~n~n'),
    % FIXME: pli bone havu apartan funkcion por sendi novan artikolon?
    once((
	Redakto = redakto,
	send_revo_redakto(Retadreso,Shangho_au_Nomo,Quoted)
	;
	Redakto = aldono,
	send_revo_aldono(Retadreso,Shangho_au_Nomo,Quoted)
	;
	%format('Status: ~d~n~n',[401]),
	throw(http_reply(html(['Neatendita eraro dum forsendo.\n'])))
    )),
    format('Bone. Sendita.').

% sintaks-kontrolo de la artikolo per Jing
revo_kontrolo(Request) :-
%%    ajax_auth(Request),
    
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
    http_open(Url,Stream,[header(content_type,ContentType),post(atom(Xml))]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).



% HTML-antaurigardo de la artikolo
revo_rigardo(Request) :-
%%    ajax_auth(Request),
    
    debug(redaktilo(request),'~q',[Request]),
    % read post data
    http_parameters(Request,
		    [
			xml(Xml, [length>100,length<500000]) % plej granda aktuale 107kB (ten.xml)
		    ]),
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    atom_codes(Xml,Codes),
    xml_quote_cdata(Codes,Quoted,ReverseEntInx,EntVal1Inx,utf8),
    % Quoted -> HTML

    %agordo:get_config(voko_xsl,VokoXsl),
    catch(
	    (
            agordo:get_path(root_dir,voko_xsl,VokoXsl),
            %sub_atom(VokoXslUri,7,_,0,VokoXsl),
            set_stream(current_output,encoding(utf8)),
            xslt_proc(VokoXsl,Quoted,Html),
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

    /**
    atom_codes(XmlAtom,Quoted),
    %format('Content-type: text/html~n~n'),
    catch(
	    (
		agordo:get_config(voko_xsl,VokoXsl),
		xsl_transform(XmlAtom,VokoXsl,Out),
		format('Content-type: text/html~n~n'),
		write(Out)
	    ),
	    error(java_exception(Exc),Class), % java_exception( classname, reference_to_exception_object),
	    (
		xsl_exception(Exc,Text),
		debug(redaktilo(rigardo),'~q: ~q',[Class,Text]),
		format('Status: ~d~n~n',[400]),
		write(Text)
	    )
	).
**/


citajho_sercho(Request) :-
%%    ajax_auth(Request),
    
    http_parameters(Request,
	    [
	    sercho(Sercho, [length>1,length<500]),
	    kie(Kie, [oneof([vikipedio,anaso,klasikaj,postaj])]) 
	    ]),

    % agordo:get_config([http_cit_scheme(CitScheme),http_cit_host(CitHost),http_cit_port(CitPort),http_cit_root(CitRoot)]),
    % ni bezonas validan AjaxID por fone demandi la citaĵo-serĉo-servon
    %request_ajax_id(Request,AjaxID),
    %once((ajax_id_time_valid(AjaxID), AxID1 = AjaxID ; new_ajax_id(Request,AxID1) )),
    % kreu la URL por fona voko de la citaĵo-servo
    %uri_authority_components(Auth,uri_authority(_,_,CitHost,CitPort)),

    agordo:get_config(http_cit_url,Url),
    uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
    atom_concat(Root,'/cikado',Path),
    uri_query_components(Search,[sercho(Sercho),kie(Kie)]),
    uri_components(Url1,uri_components(Scheme,Auth,Path,Search,_)),
    debug(redaktilo(citajho),'url ~q',[Url1]),
    http_open(Url1,Stream,[header(content_type,ContentType)]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).


% see https://commons.wikimedia.org/wiki/Special:ApiSandbox
%     https://commons.wikimedia.org/w/index.php
%     https://commons.wikimedia.org/wiki/Commons:Commons_API
%     https://commons.wikimedia.org/wiki/Commons:Credit_line
%     https://wiki.creativecommons.org/wiki/License_Versions#Detailed_attribution_comparison_chart
%     https://commons.wikimedia.org/wiki/Commons:Attribution_Generator
% /w/api.php?action=query&format=json&list=search&srsearch=korvo&srnamespace=0%7C-2&srlimit=20&srinfo=totalhits%7Csuggestion%7Crewrittenquery&srprop=size%7Cwordcount%7Ctimestamp%7Csnippet

wikimedia_pagho_limo(50).

bildo_sercho(Request) :-
    %%    ajax_auth(Request),
        
        http_parameters(Request,
            [
            sercho(Sercho, [length>1,length<500]),
            kie(_, [oneof([vikimedio])]) 
        ]),
        debug(sercho(what),'<<< VIKIMEDIO: ~w',[Sercho]),
        format('Content-type: application/json~n~n'),
        bildo_sercho_(Sercho,_).

bildo_sercho_(Sercho,JList) :-        
        uri_encoded(query_value,Sercho,SerchoEnc),
        UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
        % kreu serĉo-URL-on 
        wikimedia_pagho_limo(Max),
        % namespaces 0,6,14, see https://commons.wikimedia.org/wiki/Help:Namespaces
        format(atom(Url),'~w&list=search&srnamespace=0%7C6%7C14&srlimit=~d&srprop=snippet&srsearch=~w',[UrlBase,Max,SerchoEnc]),
        time(http_open(Url,Stream,[])),
        json_read(Stream,json(JList)),
        close(Stream),
        debug(wikimedia(search),'Url: ~w~n',[Url]),
        json_write(current_output,json(JList)).
        % kontinuus nur se venus malpli ol Max trovoj, kvankam estas pli...., verŝajne tio ne okazus
        % bildo_sercho_continuation(Url,JList).
       

% continue=json([sroffset=50,continue='-||'])
bildo_sercho_continuation(Url,JList) :-
    member(continue=json(Continue),JList) ->
        % estas pli por legi.., faru novan demandon
        wikiapi_continuation_params(Continue,ContParEnc),
        atom_concat(Url,ContParEnc,CUrl),
        time(http_open(CUrl,CStream,[])),
        % copy_stream_data(CStream,current_output)
        json_read(CStream,json(CJList)),
        close(CStream),    
        debug(wikimedia(search),'CUrl: ~w~n',[CUrl]),
        json_write(current_output,json(CJList)),
        bildo_sercho_continuation(Url,CJList) 
    ; true.

wikiapi_continuation_params([],'').    
wikiapi_continuation_params([Name=Value|Cont],ParamEnc) :-
    uri_encoded(query_value,Value,ValEnc),
    wikiapi_continuation_params(Cont,ParEnc),
    format(atom(ParamEnc),'&~w=~w~w',[Name,ValEnc,ParEnc]).


% /w/api.php?action=query&format=json&prop=imageinfo%7Cpageimages&pageids=513470%7C513472&iiprop=user%7Ccomment%7Cparsedcomment%7Ccanonicaltitle%7Ccommonmetadata%7Cextmetadata&piprop=thumbnail%7Cname%7Coriginal

bildo_info(Request) :-
    %%    ajax_auth(Request),       
        http_parameters(Request,
            [
            paghoj(Paghoj, [length>1,length<500]),
            kie(_, [oneof([vikimedio])]) 
        ]),
        debug(sercho(what),'<<< VIKIMEDIO: ~w',[Paghoj]),
        format('Content-type: application/json~n~n'),
        bildo_info_(Paghoj).

bildo_info_(Paghoj) :-    
        uri_encoded(query_value,Paghoj,PaghojEnc),
        UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
        % unue faru serĉon 
        % namespaces 0,6,14, see https://commons.wikimedia.org/wiki/Help:Namespaces
        format(atom(Url),'~w&prop=imageinfo%7Cpageimages%7Cimages%7Cinfo&inprop=url&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&iiprop=extmetadata&pageids=~w',[UrlBase,PaghojEnc]),
        time(http_open(Url,Stream,[])),
        json_read(Stream,json(JList)),!,
        close(Stream),
        debug(wikimedia(info),'Url: ~w~n',[Url]),
        write("["),
        json_write(current_output,json(JList)),!,
        bildo_info_continuation(Url,JList),
        write("]").

bildo_info_continuation(Url,JList) :-   
    member(continue=json(Continue),JList) ->
        % estas pli por legi.., faru novan demandon
        wikiapi_continuation_params(Continue,ContParEnc),
        ContParEnc \= '',
        atom_concat(Url,ContParEnc,CUrl),
        time(http_open(CUrl,CStream,[])),
        % copy_stream_data(CStream,current_output)
        json_read(CStream,json(CJList)),
        close(CStream),
        debug(wikimedia(info),'CUrl: ~w~n',[CUrl]),
        write(","), flush_output,
        json_write(current_output,json(CJList)),
        bildo_info_continuation(Url,CJList)
    ; true.
   
bildo_info_2(Request) :-
    %%    ajax_auth(Request),        
        http_parameters(Request,
            [
            dosiero(Dosiero, [length>1,length<500]),
            kie(_, [oneof([vikimedio])]) 
        ]),
        debug(sercho(what),'<<< VIKIMEDIO: ~w',[Dosiero]),
        format('Content-type: application/json~n~n'),
        bildo_info_2_(Dosiero).

bildo_info_2_(Dosiero) :-    
        uri_encoded(query_value,Dosiero,DosieroEnc),
        UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
        format(atom(Url),'~w&prop=imageinfo%7Cpageimages%7Cinfo&inprop=url&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&iiprop=extmetadata&titles=~w',[UrlBase,DosieroEnc]),
        time(http_open(Url,Stream,[])),
        json_read(Stream,json(JList)),
        close(Stream),
        debug(wikimedia(info),'Url: ~w~n',[Url]),!,
        %write("["),
        json_write(current_output,json(JList)).
        %bildo_info_continuation(Url,JList),
        %write("]").   

bildeto_info(Request) :-
    %%    ajax_auth(Request),        
        http_parameters(Request,
            [
            dosieroj(Dosieroj, [length>1,length<5000]),
            kie(_, [oneof([vikimedio])]) 
        ]),
        debug(sercho(what),'<<< VIKIMEDIO: ~w',[Dosieroj]),
        format('Content-type: application/json~n~n'),
        bildeto_info_(Dosieroj).

bildeto_info_(Dosieroj) :-    
        uri_encoded(query_value,Dosieroj,DosierojEnc),
        UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
        format(atom(Url),'~w&prop=pageimages&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&titles=~w',[UrlBase,DosierojEnc]),
        time(http_open(Url,Stream,[])),
        json_read(Stream,json(JList)),
        close(Stream),
        debug(wikimedia(info),'Url: ~w~n',[Url]),!,
        %write("["),
        json_write(current_output,json(JList)).
        %bildo_info_continuation(Url,JList),
        %write("]").       

test_wikimedia_api(Sercho) :-
    bildo_sercho_(Sercho,JList),
    member(query=json(Query),JList),
    member(search=SList,Query),
    findall(PId,
        (
          member(json(J),SList),
          member(pageid=PId,J)
        ),
        PIds
    ),
    atomic_list_concat(PIds,'|',PageIds),
    format('pageids: ~q~n',[PageIds]),
    bildo_info_(PageIds).

test_wikimedia_api_1(Paghoj) :-
    bildo_info_(Paghoj).

analinioj(Request) :-
    %%    ajax_auth(Request),
        
        %http_parameters(Request,
        %    [
        %    teksto(Teksto, [length<150000])
        %    ]),
        debug(redaktilo(analinioj),'ANA req ~q',[Request]),

        http_read_json(Request, JSON, [json_object(dict)]),

        debug(redaktilo(analinioj),'ANA json ~q',[JSON]),

        %agordo:get_config([http_ana_scheme(AnaScheme),http_ana_host(AnaHost),http_ana_port(AnaPort),http_ana_root(AnaRoot)]),
        % ni bezonas validan AjaxID por fone demandi la citaĵo-serĉo-servon
        %request_ajax_id(Request,AjaxID),
        %once((ajax_id_time_valid(AjaxID), AxID1 = AjaxID ; new_ajax_id(Request,AxID1) )),
        % kreu la URL por fona voko de la citaĵo-servo
        %uri_authority_components(Auth,uri_authority(_,_,AnaHost,AnaPort)),
        %atom_concat(AnaRoot,'/analinioj',Path),
        %uri_query_components(Query,[teksto(Teksto)]),
        %uri_components(Url,uri_components(AnaScheme,Auth,Path,'',_)),

        agordo:get_config(http_ana_url,Url),
        uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
        atom_concat(Root,'/analinioj',Path),
        uri_components(Url1,uri_components(Scheme,Auth,Path,'',_)),
        debug(redaktilo(analinioj),'url ~q',[Url1]),

        http_post(Url1, json(JSON), Reply, []),

        %%http_open(Url,ResultStream,[output(PostStream),header(content_type,ContentType)]),
        % copy data from Request to proxy connection
        %%http_read_data(Request,_,[to(PostStream)]),
        %%close(PostStream),

        debug(redaktilo(analinioj),'ANA reply ~q',[Reply]),

        % ricevu la rezultojn el la proxy-konekto kaj plusendu al la retumilo
        %%format('Content-type: ~w~n~n',['application/json']),
        %set_stream(ResultStream,encoding(utf8)),
        %set_stream(current_output,encoding(utf8)),
        %copy_stream_data(ResultStream,current_output),
        %close(ResultStream).
        reply_json(Reply).

analizo(Request) :-
    %%    ajax_auth(Request),
        
        http_parameters(Request,
            [
            teksto(Teksto, [length<150000])
            ]),
    
        %agordo:get_config([http_ana_scheme(AnaScheme),http_ana_host(AnaHost),http_ana_port(AnaPort),http_ana_root(AnaRoot)]),
        % ni bezonas validan AjaxID por fone demandi la citaĵo-serĉo-servon
        %request_ajax_id(Request,AjaxID),
        %once((ajax_id_time_valid(AjaxID), AxID1 = AjaxID ; new_ajax_id(Request,AxID1) )),
        % kreu la URL por fona voko de la citaĵo-servo
        %uri_authority_components(Auth,uri_authority(_,_,AnaHost,AnaPort)),
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
%%    ajax_auth(Request),
    set_stream(current_output,encoding(utf8)),
	
    % format('Content-type: application/json~n~n'),
    findall(Json,sqlrevo:homonimoj_sen_ref_json(Json),ResultList)
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
