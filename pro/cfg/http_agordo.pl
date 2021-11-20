:- module(http_agordo,[
    http_agordo/0,
    http_setting_info/0,
    http_path_info/0,
    http_handler_info/0
]).

:- use_module(library(uri)).
:- use_module(library(http/http_session)).

:- use_module(pro(cfg/agordo)).

http:location(red,root(red),[]).
http:location(static,root(static),[]).

http_agordo :-
	% ekstraktu la unuopaj HTTP-agordojn el la mediocariablo REDAKTILO_URL
	getenv('REDAKTILO_URL',URL) 
    -> (
        uri_components(URL,Cmp),
        uri_data(scheme,Cmp,Scheme),
        uri_data(authority,Cmp,Authority),
        uri_data(path,Cmp,AppRoot),
        uri_authority_components(Authority,ACmp),
        uri_authority_data(host,ACmp,Host),
        uri_authority_data(port,ACmp,Port),
        % metu la agordon en la kampojn de la modulo 'http:'
        set_setting(http:prefix,AppRoot),
        set_setting(http:public_scheme,Scheme),
        set_setting(http:public_port,Port),
        set_setting(http:public_host,Host)
        )
    ;   (
        writeln('AVERTO: Mankas medivariablo REDAKTILO_URL. La servo parte ne funkcios (ensalutado ktp.)'),
        AppRoot = '/'
        ),

    % la agordo de HTTP-seancoj
    get_config(http_session_timeout,SessTimeout),
    http_set_session_options([
        cookie(redaktilo_seanco),
    %%%  create(noauto),
        timeout(SessTimeout),
        path(AppRoot)
    ]),    

	% la lokaj dosierujoj el kiuj servi dosierojn
	get_path(root_dir,web_dir,WebDir),
	get_path(root_dir,voko_dir,VokoDir),
	assert(user:file_search_path(web,WebDir)),
	assert(user:file_search_path(static,web(static))),
	assert(user:file_search_path(voko,VokoDir)),
	assert(user:file_search_path(cfg,voko(cfg))),
	assert(user:file_search_path(jsc,voko(jsc))),  
	assert(user:file_search_path(stl,voko(stl))),
	assert(user:file_search_path(smb,voko(smb))).


http_path_info :-
    writeln('% http-padoj'),
    forall(
        (
            member(Path,[web,voko,static,stl,cfg,smb,js,jsc,icons,css]),
            file_search_path(Path,Dir)
        ),
        %debug(redaktilo(pado),'~q -> ~q',[Path,Dir])
        format('~q -> ~q~n',[Path,Dir])
    ). 

http_handler_info :-
    writeln('% http-peto-traktiloj'),
    http_dispatch:path_tree(T),
    forall(
        %http:location(P,H,_),
        member(node(P,H,_,_),T),
        format('~q >> ~q~n',[P,H])
    ).

http_setting_info :-
    writeln('% http-parametroj'),
    forall(
        setting(http:K,V),
        format('http:~w: ~w~n',[K,V])
    ),
    forall(
        http_session_option(O),
        format('session_opt:~w~n',O)
    ).