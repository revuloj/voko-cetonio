/* -*- Mode: Prolog -*- */
:- module(entity_dcg, [
	      entity_value//1,
	      char_ref//1
	  ]).

:- use_module(library(dcg/basics)).

/****

From XML standard https://www.w3.org/TR/2008/REC-xml-20081126/

EntityValue	   ::=   	'"' ([^%&"] | PEReference | Reference)* '"'
 |  "'" ([^%&'] | PEReference | Reference)* "'"

Entity Reference

[67]   	Reference	   ::=   	EntityRef | CharRef
[68]   	EntityRef	   ::=   	'&' Name ';'	[WFC: Entity Declared]
[VC: Entity Declared]
[WFC: Parsed Entity]
[WFC: No Recursion]
[69]   	PEReference	   ::=   	'%' Name ';'	[VC: Entity Declared]
[WFC: No Recursion]
[WFC: In DTD]

NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[4a]   	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
[5]   	Name	   ::=   	NameStartChar (NameChar)*


[66]   	CharRef	   ::=   	'&#' [0-9]+ ';'
| '&#x' [0-9a-fA-F]+ ';'	[WFC: Legal Character]


****/

entity_value([H|T]) --> char_ref(H), entity_value(T).
entity_value([H|T]) --> entity_ref(H), entity_value(T).
entity_value([H|T]) --> param_entity_ref(H), entity_value(T).
entity_value([H|T]) --> ev_char(H), entity_value(T). % fakte nun oni ne devus testi \="&","%" plu...
entity_value([]) --> eos.

param_entity_ref(par(Name)) --> "%", name(Name), ";".
entity_ref(ent(Name)) --> "&", name(Name), ";".
char_ref(C) --> "&#x", xdigits(HexDigs), ";", { hex_chr(HexDigs,C) }.
char_ref(C) --> "&#", digits(Digits), ";", { dec_chr(Digits,C) }.


name([H|T]) --> name_start_char(H), name_chars(T).

name_start_char(C) --> alpha(C).
name_start_char(0'_) --> "_".

name_chars([H|T]) --> name_char(H), name_chars(T).
name_chars([]) --> [].

ev_char(C) --> [C], { C \= "&", C \= "%" }.

name_char(C) --> alpha(C).
name_char(0'_) --> "_".
name_char(0'-) --> "-".
name_char(0'.) --> ".".
name_char(0':) --> ":".
name_char(D) --> digit(D).
name_char(C) --> [C], {C=0xb7}.
name_char(C) --> [C], {C>=0x300, C=<0x36f}.
name_char(C) --> [C], {C>=0x203f, C=<0x2040}.

alpha(A) --> [A],  { code_type(A, alpha) }.

hex_chr(Hex,Val) :-
  hex_value_(Hex,_,Val).

dec_chr(Dec,Val) :-
  dec_value_(Dec,_,Val).

hex_value_([H|T],Shift,Val) :-
  hex_value_(T,Sh,V),
  Val is H << Sh \/ V,
  Shift is Sh + 4.

hex_value_([],0,0).

dec_value_([H|T],Fact,Val) :-
  dec_value_(T,F,V),
  Val is (H-0'0) * F + V,
  Fact is F*10.

dec_value_([],1,0).
