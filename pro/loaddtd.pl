/* -*- Mode: Prolog -*- */
:- module(loaddtd,[
        load_dtd/1, 
	      load_entities/1,
	      dtd2pl_entities/0,
        dtd2json_entities/0 % tion nun povas ankaŭ fari voko-grundo/js_util/vokosgn2x.js
    ]).

:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- use_module(library(pairs)).
:- use_module(entity_dcg).

dtd('../dtd/vokoxml.dtd').
prolog_entity_file('voko_entities.pl').
json_entity_file('voko_entities.js'). 


load_dtd(VokoDTD) :-
  % PLIBONIGU:
  % se dosiero ne ekzistas oni povus preni per HTTP...
  % antaŭŝargu DTD
  dtd(DTDFile),
  new_dtd(vortaro,VokoDTD),
  load_dtd(VokoDTD,DTDFile,[dialect(xml)]).

%% Testoj...

%entity_list(DTD,Entities) :-
%  dtd_property(DTD,Entities).
t(1,`<!ENTITY % signoj SYSTEM "../dtd/vokosgn.dtd">`).
t(2,`<!ENTITY % priskribaj-elementoj\c\n
  "fnt|gra|uzo|dif|ekz|rim|ref|refgrp|trd|trdgrp|bld|adm|url|mlg|lstref|tezrad">`).

t(3,[`OElig`-`&#x0152;`]).

t(4,[`Alfa`-`&#x391;`,`alfa`-`&#x3b1;`,`alfa_acute`-`&alfa_ton;`,
	`alfa_subj`-`&alfa;`,`Alfa_Subj`-`&Alfa;`,`Alfa_ton`-`&#x0386;`,`alfa_ton`-`&#x03ac;`]).


test(N,E) :-
  t(N,T),
  phrase(dtd(E),T).

test_resolve(N,R) :-
  t(N,E),
  entity_resolution(E,R).


dtd2pl_entities:-
  load_entities(EL),
  prolog_entity_file(File),
  save_entities(File,EL).

dtd2json_entities:-
  load_entities(EL),
  json_entity_file(File),
  save_entities_json(File,EL).

load_entities(EntityList) :-
  dtd(DTDFile),
  load_entities_from_file(DTDFile,EntityList),!.

load_entities_from_file(File,EntityList) :-
   phrase_from_file(dtd(EL1),File),
   retrieve_entities(EL1,EL2),!,
%   print(EL2),
   entity_resolution(EL2,EntityList).

save_entities(FileName,EntityList) :-
  setup_call_cleanup(
    open(FileName,write,Out,[encoding(utf8)]),
    (
      writeln(Out,':-encoding(utf8).'),
      with_output_to(Out,
        write_entities(EntityList))
    ),  
    close(Out)
  ).

save_entities_json(FileName,EntityList) :-
  setup_call_cleanup(
    open(FileName,write,Out,[encoding(utf8)]),
    (
      writeln(Out,'/* jshint esversion: 6 */'),
      writeln(Out,'const voko_entities={'),
      json_write_entries(EntityList,Out),
      writeln(Out,'}')
    ),
    close(Out)
  ).

json_write_entries([Key-Value],Out) :-
  json_write_entry(Key,Value,Out),!.

json_write_entries([Key-Value|Rest],Out) :-
  atom_length(Value,L), (L=1;L>10), !,
  json_write_entry(Key,Value,Out),
  write(Out,",\n"),
  json_write_entries(Rest,Out).

% ni ne eligas unuojn kun longeco>1 en JSON
json_write_entries([_|Rest],Out) :-
  json_write_entries(Rest,Out).

json_write_entry(Key,_,Out) :-
  member(Key,[`amp`,`lt`,`gt`,`apos`,`quot`]),!,
  format(Out,'"~s":"&~s;"',[Key,Key]).

json_write_entry(Key,Value,Out) :-
  format(Out,'"~s":"~s"',[Key,Value]).

retrieve_entities([entity(E)|Tail],[E|EntityList]) :-
  retrieve_entities(Tail,EntityList).

retrieve_entities([system(_-Url)|Tail],EntityList) :-
  phrase_from_file(dtd(EL),Url),
  retrieve_entities(EL,EntityList1),
  retrieve_entities(Tail,EntityList2),
  append(EntityList1,EntityList2,EntityList).
  
retrieve_entities([_|Tail],EntityList) :-
  retrieve_entities(Tail,EntityList).

retrieve_entities([],[]).

predefined_entities([`amp`-[38], `lt`-[60], `gt`-[62], `apos`-[39], `quot`-[34]]).

% it is assumed that entity values containt entity references already resolved only
% should be checkd and or iterated otherwise, alternatively split enity list into
% fully resolved and those containing entity references till the second list is empty
entity_resolution(Entities,Resolved) :-
  parse_entities(Entities,Parsed),
  predefined_entities(Predef),
  append(Predef,Parsed,EL),
  resolve_entities(EL,Resolved).


% first step of entity value resolution is parsing it into chars and entity references
parse_entities([Name-Value|Tail],[Name-Parsed|ResTail]) :-
  phrase(entity_value(Parsed),Value), !,
%  format('~s - ~k~n',[Name,Parsed]),
  parse_entities(Tail,ResTail).
parse_entities([],[]).

% second step of entity value resolution is resolving entity references to their value
resolve_entities(ParsedEntities,Resolved) :-
  list_to_assoc(ParsedEntities,Index),
  maplist(resolve_entity(Index),ParsedEntities,Resolved).

resolve_entity(Index,Key-Value,Key-Resolved) :-
  resolve_entity_value(Index,Value,Resolved),
  format('~s - ~s~n',[Key,Resolved]).

resolve_entity_value(Index,[ent(Name)|T],Resolved) :-
  get_assoc(Name,Index,Res),
  resolve_entity_value(Index,T,RT),
  append(Res,RT,Resolved).

resolve_entity_value(Index,[H|T],[H|RT]) :- 
  integer(H),
  resolve_entity_value(Index,T,RT).

resolve_entity_value(_,[],[]).

write_entities([Name-Value|Tail]) :-
  atom_codes(AName,Name), 
  atom_codes(AValue,Value), 
  write_term(entity(AName,AValue),
	[quoted(true),fullstop(true),nl(true)]),
  write_entities(Tail).
write_entities([]).



/**********
quick and dirty scanning DTD for entity declarations
**********/

dtd([]) --> eos.

dtd([H|T]) -->
  decl(H), !,
%  { format('decl: ~w~n',[H]) },
% { portray_clause(H) },
  dtd(T).

dtd(T) -->
  any, !,
  dtd(T).


decl(entity(Name-Value)) -->
  "<!ENTITY", blanks, name(Name), blanks, """", value(Value), """>", blanks.

decl(param(Name-Value)) -->
  "<!ENTITY", blanks, "%", blanks, name(Name), blanks, """", value(Value), """>", !, blanks.

decl(system(Name-Url)) -->
  "<!ENTITY", blanks, "%", blanks, name(Name), blanks, "SYSTEM", !, blanks, """", url(Url), """>", blanks.

decl(exception) -->
  "<!ENTITY", !, string_without(">",S), { atom_codes(A,S), throw(could_not_parse_entity(A)) }.

% fall back don't read declarations which are not imports or entities
any --> blanks, "<?", !, string_without("?>",_), "?>", blanks.
any --> "<!--", any_chars, "-->", blanks.
any --> "<!ELEMENT", !, string_without(">",_), ">", blanks.
any --> "<!ATTLIST", !, string_without(">",_), ">", blanks.
any --> string_without("<",S), { S\= [] }.

any --> !, % ne rekuru post eraro!
        call(parse_error_('nevalida enhavo')).

any_chars --> [].
any_chars --> [_], any_chars.

name(Name) --> namesigns(Name). %, { atom_codes(Name,Letters) }.

value(Value) --> string_without(""">",Value). %, { atom_codes(Value,Codes) }.

url(Url) --> value(Url).

namesigns([H|T]) -->
        [H],
        { code_type(H, csym)
        }, !,
        namesigns(T).
namesigns([H|T]) -->
        "-", !,
        { char_code('-',H) },
        namesigns(T).
/*
namesigns([H|T]) -->
        "_", !,
        { char_code('_',H) },
        namesigns(T).
*/
namesigns([]) -->
        [].



parse_error_(Msg,Rest,Rest) :- !,
  once(( % ne kreu saman eraron diversmaniere...
        first_part_of_list(20,Rest,Begin),
        format('~w: ~s~n',[Msg,Begin]),
        throw(Msg)
%        assertz(dtd_parse_error(Msg,Begin))
  )).

first_part_of_list(Count,List,List) :-
  length(List,ListLen),
  ListLen < Count.

first_part_of_list(Count,List,Head) :-
  length(Head,Count),
  append(Head,_,List).


%user:sgml_write_node_hook(comment(C),Out,State) :-
%  write(Out,C).


%%% from sgml_write...
/***
dtd_character_entities(DTD, Map) :-
        empty_assoc(Empty),
        dtd_property(DTD, entities(Entities)),
        fill_entity_map(Entities, DTD, Empty, Map).

fill_entity_map([], _, Map, Map).
fill_entity_map([H|T], DTD, Map0, Map) :-
        (   dtd_property(DTD, entity(H, CharEntity)),
            atom(CharEntity),
            (   sub_atom(CharEntity, 0, _, _, '&#'),
                sub_atom(CharEntity, _, _, 0, ';')
            ->  sub_atom(CharEntity, 2, _, 1, Name),
                atom_number(Name, Code)
            ;   atom_length(CharEntity, 1),
                char_code(CharEntity, Code)
            )
        ->  put_assoc(Code, Map0, H, Map1),
            fill_entity_map(T, DTD, Map1, Map)
        ;   fill_entity_map(T, DTD, Map0, Map)
        ).
***/
