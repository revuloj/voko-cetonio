/* -*- Mode: Prolog -*- */

:- module(agordo,[
	      get_config/2
	  ]).

:- dynamic send_pw/1, revodb/1.

% apriora agordo-dosierujo, povas esti
% ŝanĝita per swipl -p agordo=/...
% user:file_search_path(agordo,'/home/revo/etc').
    
:- initialization(read_cfg).


% sha_hash(wolfram,H,[algorithm(sha384),encoding(utf8)]),hash_atom(H,A).

read_cfg :-
    once((
	 getenv('HOME',HomeDir),
	 atom_concat(HomeDir,'/etc/redaktilo.cfg',CfgFile),
	 exists_file(CfgFile)
	 ;
	 expand_file_search_path(agordo('redaktilo.cfg'),CfgFile)
	)),
    ensure_loaded(CfgFile).


get_config(Key,Value) :-
    atom(Key),
    call(agordo:Key,Value).

get_config([]).
get_config([Opt|KVs]) :-
%    Opt =.. [Key,Value],
    %    call(agordo:Key,Value),
    call(Opt),
    get_config(KVs).
