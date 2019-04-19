/* -*- Mode: Prolog -*- */
:- module(tokenize,[
	      tokenize/3,
	      tokenize_file/3,
	      lookahead/3,
              first_part_of_list/3,
	      output/1
	  ]).

:- use_module(library(dcg/basics)).


tokenize(Text,Tokens,Lines) :-
	phrase(alltokens(Tokens,Lines),Text),!.

tokenize_file(File,Tokens,Lines) :-
  phrase_from_file(alltokens(Tokens,Lines),File),!.

lookahead(T), T --> T.
%lookahead(T1), [T] --> T.

first_part_of_list(Count,List,List) :-
  length(List,ListLen),
  ListLen < Count.

first_part_of_list(Count,List,Head) :-
  length(Head,Count),
  append(Head,_,List).


%%%%%%%%%%
% lexer (tokenizer) rules

lh_gt --> lookahead(">").
lh_col --> lookahead(":").
lh_lbr --> lookahead("{").

delimiters(" \n\r\t\\~,{}<>:[]").
cmnt_delim("\n\r->").
%listdelimiters(" \n\r\t\\{},:[]").

gt --> ">".
lt --> "<".
col --> ":".
lbr --> "{".
rbr --> "}".

tag(vortaro) --> "vortaro".
tag(art) --> "art".
tag(subart) --> "subart".
tag(drv) --> "drv".
tag(subdrv) --> "subdrv".
tag(snc) --> "snc".
tag(subsnc) --> "subsnc".

tag(dif) --> "dif".
tag(ekz) --> "ekz".
tag(rim) --> "rim".

tag(fnt) --> "fnt".
tag(aut) --> "aut".
tag(bib) --> "bib".
tag(vrk) --> "vrk".
tag(lok) --> "lok".
tag(url) --> "url".

tag(img) --> "img".
tag(svg) --> "svg".

tag(ofc) --> "ofc".
tag(kap) --> "kap".
tag(rad) --> "rad".

tag(fak) --> "fak".
tag(uzo) --> "uzo".
tag(reg) --> "reg".
tag(stl) --> "stl".

tag(mlg) --> "mlg".

tag(ref) --> "ref".
tag(vid) --> "vid".
tag(super) --> "super".
tag(sub) --> "sub".
tag(malprt) --> "malprt".
tag(prt) --> "prt".
tag(rex) --> "rex".
tag(rdf) --> "rdf".
tag(sin) --> "sin".
tag(ant) --> "ant".
tag(lst) --> "lst".

tag(trd) --> "trd".
tag(trdgrp) --> "trdgrp".
tag(trdj) --> "trdj".

tag(ind) --> "ind".
tag(amb) --> "amb".
tag(klr) --> "klr".

% KOREKTU: kontrolu la lingvojn laŭ tabelo
lng(Codes,Lng) :- atom_codes(Lng,Codes).


spacoj([C|Rest]) -->
	spaco(C),!,
	spacoj(Rest).
spacoj([]) --> [].

spaco(C) -->
        [C],
        { nonvar(C),
          code_type(C, white)
        }.



token([start(Tag),gt]), [Line] --> [Line], tag(Tag), gt.
token([start(Tag),col,attr(Attr),gt]), [Line] --> [Line], tag(Tag), col, string_without(">",A), gt, { string_codes(Attr,A) }.
% KOREKTU: ĉu dishaki atributojn ĉe ","?

token([lbr,start(Tag),gt]), [Line] --> [Line], lbr, tag(Tag), gt.
token([lbr,start(Tag),col,attr(Attr),gt]), [Line] --> [Line], lbr, tag(Tag), col, string_without(">",A), gt, { string_codes(Attr,A) }.
token([lt,end(Tag)]), [Line] --> [Line], lt, tag(Tag).
token([rbr]), [Line] --> [Line], rbr.

token([cstart]), [Line] --> [Line], "<!--".
%token([cend]), [Line] --> [Line], "-->".

token([blst,start(Tag),gt]), [Line] --> [Line], "[[", tag(Tag), gt.
%token([elst]) --> "]]".

%%%token([col,attr(Attr),gt]), [Line] --> [Line], col, string_without(">",A), gt, { string_codes(Attr,A) }.
% KOREKTU: cu dishaki atributojn ĉe ","?
%token([col,attr(Attr)]) --> col, string_without("{",A), lh_lbr, { string_codes(Attr,A) }.

%token([lbr,text(Text),rbr]) --> lbr, string_without(" }<",T), rbr, { string_codes(Text,T) }.
token([nl(Line1)]), [Line1] --> [Line], whites, "\n", { Line1 is Line+1 }. % dume forigu linifinajn troajn spacojn
% token([wht]), [Line] --> [Line], white, whites. % kunigu plurajn
% spacojn al unu
% KOREKTO: lasu plurajn spacojn ĉe la komenco por
% senŝovado...?
token([wht(Spc)]), [Line] --> [Line], spaco(S), spacoj(Sj), { atom_codes(Spc,[S|Sj]) }. % konservu spacojn, almenaŭ linikomence necesas
% alternative oni povus kalkuli la nombron kaj redoni ekz. [wht(5)]

token([tld]), [Line] --> [Line], "~".
token([com]), [Line] --> [Line], ",".
token([col]), [Line] --> [Line], ":".
token([text(Text)]), [Line] --> [Line], {delimiters(Delim)}, string_without(Delim,T), { T \= [], string_codes(Text,T) }.

% fall back to error if no other matches
token([]) --> call(lex_error_).

cmnttoken([cend]), [Line] --> [Line], "-->".
cmnttoken([nl(Line1)]), [Line1] --> [Line], whites, "\n", { Line1 is Line +1 }.
cmnttoken([ctext(Text)]), [Line] --> [Line], {cmnt_delim(Delim)}, 
	string_without(Delim,T), { T \= [], string_codes(Text,T) }.
cmnttoken([ctext("-")]), [Line] --> [Line], "-".

% ene de listo (trdj) uzu alian sintakson

listtoken([nl(Line1)]), [Line1] --> [Line], whites, "\n", { Line1 is Line +1 }.
% dume forigu linifinajn troajn spacojn listtoken([wht]),
% [Line] --> [Line], white, whites. % kunigu plurajn spacojn al unu
% listtoken([wht]), [Line] --> [Line], white. % konservu spacojn, almenaŭ
% linikomence necesas alternative oni povus kalkuli la nombron kaj redoni
% ekz. [wht(5)]
listtoken([wht(Spc)]), [Line] --> [Line], spaco(S), spacoj(Sj), { atom_codes(Spc,[S|Sj]) }.

listtoken([elst]), [Line] --> [Line], "]]".

listtoken([lng(Lng),col]), [Line] --> [Line], [L1,L2],":", { lng([L1,L2],Lng) }.
listtoken([lng(Lng),col]), [Line] --> [Line], [L1,L2,L3],":", { lng([L1,L2,L3],Lng) }.
listtoken([text(Text)]), [Line] --> [Line], {delimiters(Delim)}, string_without(Delim,T), { T \= [], string_codes(Text,T) }.
listtoken([com]), [Line] --> [Line], ",".

listtoken([lbr,start(Tag),gt]), [Line] --> [Line], lbr, tag(Tag), gt.
listtoken([lbr,start(Tag)]), [Line] --> [Line], lbr, tag(Tag), lh_col.
listtoken([rbr]), [Line] --> [Line], rbr.

listtoken([col,attr(Attr),gt]), [Line] --> [Line], col, string_without(">",A), gt, { string_codes(Attr,A) }.

listtoken([lsq]), [Line] --> [Line], "[".
listtoken([rsq]), [Line] --> [Line], "]".

% fall back to error if no other matches
listtoken([]) --> call(lex_error_).


lex_error_([Line|Rest],Rest) :-
  % length(Rest,L), throw(error(syntax_error_at,Rest,L)).
  first_part_of_list(50,Rest,Begin),
  atom_codes(Str,Begin),
  throw(syntax_error_at(Line,Str)).

% PLIBONIGU: nombru linifinojn aŭ dum la analizado aŭ antaŭe registru ĉiujn
% kaj informu laŭ pozicio; se parse_from_file uzu line_count, line_position

alltokens(Tokens,Lines) --> start_line1, tokens(Tokens), [Lines].
start_line1,[1] -->[].

tokens([]), [Lines] --> [Lines], eos.
tokens(Tokens) -->
	token(T),
	{
          T\=[]
%%%,
%%%          format('~w ',[T]),
%%%          (T = [nl(_)] -> format('~n'); true)
        },
        % uzu alian sintakson ene de listo [[...]]
        ({ T = [blst|_] }
          ->
          listtokens(Rest)
          ;
	  { T = [cstart|_] }
          ->
          cmnttokens(Rest)
	  ;
	  tokens(Rest)
        ),
	{ append(T,Rest,Tokens) }.

cmnttokens(Tokens) -->
	cmnttoken(T),
	{
          T\=[]
%%%,
%%%          format('- ~w ',[T]),
%%%          (T = [nl(_)] -> format('~n'); true)
        },
	({ T = [cend|_] }
          ->
          tokens(Rest)
          ;
          cmnttokens(Rest)
        ),
        { append(T,Rest,Tokens) }.

% KOREKTU: momente commento ne estas permesita en listo...!?
listtokens(Tokens) -->
	listtoken(T),
	{
          T\=[]
%%%,
%%%          format('- ~w ',[T]),
%%%          (T = [nl(_)] -> format('~n'); true)
        },
	% uzu alian sintakson ekster listo [[...]]
	({ T = [elst|_] }
          ->
          tokens(Rest)
          ;
          listtokens(Rest)
        ),
        { append(T,Rest,Tokens) }.


%%%%%%%%%%%%%%%
% output tokens - precipe por kontrolo(?)

output([]) :- !. % ne reprovu/rekuru post fino de "output"
output([Token|Rest]) :-
  output_(Token),
  output(Rest).

output_(start(Tag)) :- write(Tag).
output_(col) :- write(':').
output_(attr(Attr)) :- write(Attr).
output_(gt) :- write('>').
output_(lt) :- write('<').
output_(end(Tag)) :- write(Tag).
output_(lbr) :- write('{').
output_(rbr) :- write('}').
output_(blst) :- write('[[').
output_(elst) :- write(']]').
output_(text(Text)) :- write(Text).
output_(lng(Lng)) :- write(Lng).
output_(tld) :- write('~').
output_(wht) :- write(' ').
output_(wht(W)) :- write(W).
output_(nl(_)) :- write('\n').
output_(com) :- write(',').
output_(lsq) :- write('[').
output_(rsq) :- write(']').
output_(cstart) :- write('<!--').
output_(ctext(Text)) :- write(Text).
output_(cend) :- write('-->').

% fallback to this for debugging instead of error
output_(Unexpected) :- !, format('<<~w>>',Unexpected).

% fallback to error
%output_(T) :- throw(neatendita_signo_esprimo(T)).

