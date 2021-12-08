/* -*- Mode: Prolog -*- */

:- module(agordo,[
	      get_config/2,
		  get_path/3
	  ]).


:- dynamic send_pw/1, revodb/1, env_supersede/1, secrets/1.

% apriora agordo-dosierujo, povas esti
% ŝanĝita per swipl -p agordo=/...
% user:file_search_path(agordo,'/home/revo/etc').
    
:- initialization(read_cfg).

% sha_hash(wolfram,H,[algorithm(sha384),encoding(utf8)]),hash_atom(H,A).

read_cfg :-
	read_cfg('redaktilo.cfg'),
	%read_cfg('redaktilo.url'),
	read_cfg('oauth_setup'),
	read_secrets('redaktilo.skr').

read_cfg(FileName) :-
    once((
		% agordo-dosiero povas esti en ~/etc/
		getenv('HOME',HomeDir),
		atomic_list_concat([HomeDir,'etc',FileName],'/',CfgFile),
		exists_file(CfgFile)
		;
		% aŭ ĝi estas agordita per komandlinia parametro: -p agordo=/ie/cfg
		% PLOBONIGU: ĉu ni serĉu tie unue?
		expand_file_search_path(agordo(FileName),CfgFile)
	)),
	format('Legante agordon el ~w...~n',[CfgFile]),
	ensure_loaded(CfgFile). 

read_secrets(DefaultSecretFN) :-
	once((	  
		% la agordo-predikato 'secrets' indikas kie estas la sekreto-dosiero
		get_config(secrets,SecretFile),
		exists_file(SecretFile),
		format('Legante agordon el ~w...~n',[SecretFile]),
		ensure_loaded(SecretFile)
		;
		% se ĝi ne estas agordita aŭ ne troviĝas en la indikita loko
		% ni provu trovi ĝin en aprioraj agordo-dosierujoj
		read_cfg(DefaultSecretFN)
	)).

% agordoj, kiujn ni
% povas legi el medio-variablo kun resp. majuskla nomo
get_config(Key,Value) :-
    atom(Key), upcase_atom(Key,KEY),
	call(agordo:env_supersede,ESS),
	memberchk(KEY,ESS),	
	getenv(KEY,Value),!.

% donu unu agordo-valoron per gia ŝlosilo
get_config(Key,Value) :-
    atom(Key),
    call(agordo:Key,Value).



% donu liston de agordoj en la formo [nomo1(Valoro1),nomo2(Valoro2),...]
get_config([]).
get_config([Opt|KVs]) :-
%    Opt =.. [Key,Value],
    %    call(agordo:Key,Value),
    call(Opt),
    get_config(KVs).

% pado estas kunmetita el agordo por RootKey kaj agordo por PathKey
get_path(RootKey,PathKey,Value) :-
	get_config(RootKey,Root),
	get_config(PathKey,Path),
	atom_concat(Root,Path,Value).
