/* -*- Mode: Prolog -*- */
:- module(parsexml,[
         parsexml/3,
	 is_dom/1
     ]).

:- use_module(library(sgml)).

test_file('abel.xml').

dtd('../dtd/vokoxml.dtd').
%dtd('../dtd/vokosgn.dtd').


test() :-
  test_file(File),
  open(File,read,Stream,[]),
  load_dtd(DTD),
time(
  parsexml(Stream,DTD,Tree)
),
  close(Stream),
  %nb_getval(dom,X), 
  nl, writeq(Tree),
  is_dom(Tree).


parsexml(Stream,DTD,DOM1) :-
  nb_setval(dom,_),
  nb_delete(parser_pos),
  set_stream(Stream,encoding(utf8)),
  new_sgml_parser(Parser,[dtd(DTD)]),
  %save_parser_pos(Parser), has no stream yet...
  sgml_parse(Parser,[
        source(Stream),
	dialect(xml),
	call(begin,on_begin),
	call(end,on_end),
	call(decl,on_decl),
	call(cdata,on_cdata)
    ]),
  nb_getval(dom,DOM1),
  term_variables(DOM1,[Pos|_]),  
  Pos = [],
  free_sgml_parser(Parser).

load_dtd(VokoDTD) :-
  % PLIBONIGU:
  % se dosiero ne ekzistas oni povus preni per HTTP...
  % antaŭŝargu DTD
  dtd(DTDFile),
  new_dtd(vortaro,VokoDTD),
  load_dtd(VokoDTD,DTDFile,[dialect(xml)]).


on_begin(Tag,Attr,Parser) :-
  save_parser_pos(Parser),
  nb_getval(dom,DOM), 
  term_variables(DOM,[Pos|_]),  
  Pos = [element(Tag,Attr,_Content)|_Tail],
  nb_setval(dom,DOM).
%,  format('<~w ~w>',[Tag,Attr]).

on_end(_Tag,Parser) :-
  save_parser_pos(Parser),
  nb_getval(dom,DOM),
  term_variables(DOM,[Pos|_]),  
  Pos = [],
  nb_setval(dom,DOM).
%,  format('</~w>',[Tag]).

on_decl(_,Parser) :-
  % tio fiaskas la unuan fojon, sed la dokumenttipdeklaron ni ne bezonas...
  parser_last_pos(Parser,CurrentPos),

  new_memory_file(CommentFile),
  open_memory_file(CommentFile,write,CommentStr),
  set_stream(CommentStr,encoding(utf8)),

  get_sgml_parser(Parser,source(Stream)),
  get_char(Stream,_), % PROVIZORE: la analizila lasta pozicio montras al signo antaŭ la komento, 
					    % sed mi ne scias, ĉu ĉiam...?
  get_parser_current_len(Parser,CommentLen),
  copy_stream_data(Stream,CommentStr,CommentLen),
  set_stream_position(Stream,CurrentPos),
  save_parser_pos(Parser),

  close(CommentStr),
  memory_file_to_atom(CommentFile,Cmt),

  (
     atom_concat('<!--',Cmt1,Cmt),
     atom_concat(Comment,'-->',Cmt1)
     -> true
     ;
	% KOREKTU: se estas aliaj deklaroj <? ...?> aŭ <![[...]]> traktu tion antaŭ ĵeti escepton
	% necesas eltesti kio povas okazi aŭ analizi swi-prolog/packages/sgml/parse.c 
     stream_pos(CurrentPos,Line,LineChar),
     throw(not_a_comment_or_seek_error(Line:LineChar,Cmt))
  ),

  nb_getval(dom,DOM),
  term_variables(DOM,[Pos1|_]),  
  Pos1 = [comment(Comment)|_Tail],
  nb_setval(dom,DOM).
%,    format('~w',[Comment])
  

on_cdata(CDATA,Parser) :-
  save_parser_pos(Parser),
  nb_getval(dom,DOM),
  term_variables(DOM,[Pos|_]),  
  Pos = [CDATA|_Tail],
  nb_setval(dom,DOM).

%,  format('~w',[CDATA]).

get_parser_current_len(Parser,Len) :-
   get_sgml_parser(Parser,charpos(Start,End)),
   Len is End-Start.

save_parser_pos(Parser) :-
  get_sgml_parser(Parser,source(Stream)),
  stream_property(Stream,position(Position)),
  nb_setval(parser_pos,Position).

parser_last_pos(Parser,CurrentPosition) :-
  % get current position
  get_sgml_parser(Parser,source(Stream)),
  stream_property(Stream,position(CurrentPosition)),
  % find out last position, first time use CurrentPostion instead
  nb_current(parser_pos,Position),
  %; Position = CurrentPosition),
  set_stream_position(Stream,Position).

/**
stream_pos(Stream,PosObj,LineCnt,LinePos,CharCnt,ByteCnt) :-
  stream_property(Stream,position(PosObj)),
  stream_position_data(line_count,PosObj,LineCnt),
  stream_position_data(line_position,PosObj,LinePos),
  stream_position_data(char_count,PosObj,CharCnt),
  stream_position_data(byte_count,PosObj,ByteCnt).
**/

stream_pos(Position,Line,LineChar) :-
  stream_position_data(line_count,Position,Line),
  stream_position_data(line_position,Position,LineChar).

is_dom(0) :- !, fail.               % catch variables
is_dom(List) :-
        is_list(List), !,
        is_content_list(List).
is_dom(Term) :-
        is_element(Term).

is_content_list([]).
is_content_list([H|T]) :-
        is_content(H),
        is_content_list(T).

is_content(0) :- !, fail.
is_content(pi(Pi)) :- !,
        atom(Pi).

% enhavo speciale enŝovita de transformilo
is_content(comment(Cmt)) :- !,
        atom(Cmt).
is_content(content(Cnt)) :- !,
        atom(Cnt).

is_content(CDATA) :-
        atom(CDATA), !.
is_content(CDATA) :-
        string(CDATA), !.
is_content(Term) :-
        is_element(Term).

is_element(element(Name, Attributes, Content)) :-
        dom_name(Name),
        dom_attributes(Attributes),
        is_content_list(Content).

dom_name(NS:Local) :-
        atom(NS),
        atom(Local), !.
dom_name(Local) :-
        atom(Local).

dom_attributes(0) :- !, fail.
dom_attributes([]).
dom_attributes([H|T]) :-
        dom_attribute(H),
        dom_attributes(T).

dom_attribute(Name=Value) :-
        dom_name(Name),
        atomic(Value).
