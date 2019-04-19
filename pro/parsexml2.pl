/* -*- Mode: Prolog -*- */
:- module(parsexml2,[
         parsexml/2,
	 is_dom/1
     ]).

% http://binding-time.co.uk/wiki/index.php/Parsing_XML_with_Prolog
% http://binding-time.co.uk/wiki/index.php/XML_Module

:- use_module(entities).
%:- use_module(library(sgml)).
:- use_module('xmlpl/xml.pl').
:- use_module(xml_quote).



test_file('abel.xml').

dtd('../dtd/vokoxml.dtd').
%dtd('../dtd/vokosgn.dtd').


test() :-
  test_file(File),
  open(File,read,Stream,[]),
%  load_dtd(DTD),
time(
  parsexml(Stream,Tree)
),
  close(Stream),
  %nb_getval(dom,X), 
  nl, writeq(Tree),
  is_dom(Tree).


parsexml(Stream,DOM1) :-
    is_stream(Stream),
    set_stream(Stream,encoding(utf8)),
    read_stream_to_codes(Stream,Codes),
    parsexml(Codes,DOM1).

parsexml(Text,DOM1) :-
  entity_pairs(entities:entity,Pairs),
  xml_parse([entities(Pairs)],Text,DOM1).



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


