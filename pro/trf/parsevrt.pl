/* -*- Mode: Prolog -*- */
:- module(parsevrt,[
	      read_vrt/3,
	      read_vrt_from_file/2,
	      parse_to_xmldom/3
	  ]).


:- use_module(library(dcg/basics)).
:- ensure_loaded(parsexml).
:- ensure_loaded(vrtsign).
:- ensure_loaded(transform).

% vrt = voko-redakto-transformo

:- dynamic vrt_parse_error/3.


read_vrt(Text,VrtTree,XmlDOM) :-
  tokenize(Text,Tokens,Lines),!,
  format('~n% procedis ~d liniojn, analizonte la strukturon...~n',[Lines]),
  parse_to_xmldom(Tokens,VrtTree,XmlDOM).

read_vrt_from_file(File,XmlDOM) :-
  tokenize_file(File,Tokens,Lines),!,
  format('~n% procedis ~d liniojn, analizonte la strukturon...~n',[Lines]),
  parse_to_xmldom(Tokens,_,XmlDOM).

read_vrt_from_file(File,VrtTree,XmlDOM) :-
  tokenize_file(File,Tokens,Lines),!,
  format('~n% procedis ~d liniojn, analizonte la strukturon...~n',[Lines]),
  parse_to_xmldom(Tokens,VrtTree,XmlDOM).

parse_to_xmldom(Tokens,VrtTree,XmlDOM) :-
  parse(Tokens,VrtTree),!,
  format('% transformonte la strukturon...~n',[Lines]),
  trf_list(VrtTree,XmlDOM),!,
  format('% kontrolonte la strukturon...~n',[Lines]),
  % kontrolu rezulton
  trf_is_dom(XmlDOM),
  format('% BONE!~n').

trf_is_dom(XmlDOM) :-
  ground(XmlDOM) -> parsexml:is_dom(XmlDOM)
    ; check_free_variables(XmlDOM).

% trovu liberan variablon kaj lokon 
check_free_variables(Term) :-
   numbervars(Term,0,N), N>0,
   with_output_to(atom(Atom), write_canonical(Term)),
   sub_atom(Atom,Before,_,_,'$VAR'),
   Start is Before-20,
   sub_atom(Atom,Start,60,_,Context),
   throw(transform_free_vars(N,Context)).


parse(Tokens,Tree) :-
    phrase(parse(Tree),Tokens)
      -> show_errors(10)
      ; false.

show_errors(N) :-
  findall(Line-[Msg,Tokens],vrt_parse_error(Line,Msg,Tokens),Errs),
  keysort(Errs,ErrsSorted),
  show_errors(N,ErrsSorted).

show_errors(_,[]).
show_errors(N,[Line-[Msg,Tokens]|Rest]) :-
  N>0, N1 is N-1,
  output_error(Line,Msg,Tokens),
  show_errors(N1,Rest).


/* *
show_errors(_) :-
  \+ vrt_parse_error(_,_,_), !.

show_errors(Cnt) :-
  Cnt>0,
  vrt_parse_error(Line,Msg,ErrList),
  output_error(Line,Msg,ErrList),
  Cnt1 is Cnt-1,
  show_errors(Cnt1).
* */

n_errors(0,List,List) :- !.
n_errors(N,List,Errs) :-
	vrt_parse_error(Line,Msg,Tokens),
	(member([Line,Msg,Tokens],List) -> fail; true),
	N1 is N - 1,
	n_errors(N1,[[Line,Msg,Tokens]|List],Errs).
n_errors(N,Errs) :- n_errors(N,[],Errs).



output_error(Line,Msg,ErrList) :-
  once((
   format('% ERARO ĉe linio ~d: ~w:~n',[Line,Msg]),
   output(ErrList),
   format('~n')
  )).





%%%%%%%%%%
% parser rules

%  `art::mirh> ofc{*}mirh/o\c\n
%  drv> ~o
%    snc>Rezino bonodora<snc
% <drv
% <art`).

parse(Tree) -->
	{ retractall(vrt_parse_error(_,_,_)) },
	set_line(1),
%	element(Tree),
        content(Tree),
	[_Lines].

% fall back to error if no other matches
%parse([],Error) --> { throw(could_not_parse) }.
set_line(Line), [Line] --> [].


%element([]), [Line] --> [Line], eos.

element(element(Tag,[],Content)), [Line1] -->
	[Line],
	[start(Tag),gt], !,
	set_line(Line),
	content(Content),
	[Line1],
	[lt, end(Tag)].

element(element(Tag,Attr,Content)), [Line1] -->
	[Line],
	[start(Tag), col, attr(AttrCodes), gt], !,
        { atom_codes(Attr,AttrCodes) },
	set_line(Line),
	content(Content),
	[Line1],
	[lt, end(Tag)].

element(element(Tag,[],Content)), [Line1] -->
	[Line],
	[lbr, start(Tag), gt], !,
	set_line(Line),
	content(Content),
	[Line1],
	[rbr].


element(element(Tag,Attr,Content)), [Line1] -->
	[Line],
	[lbr, start(Tag), col, attr(AttrCodes), gt], !,
        { atom_codes(Attr,AttrCodes) },
	set_line(Line),
	content(Content),
	[Line1],
	[rbr].


% KOREKTU atributon deduktu el komenclitero antau ~
%element(element(tld,[],[])), [Line] -->
%	[Line],
%	[tld].


element(lst(Tag,[],Content)), [Line1] -->
	[Line],
	[blst, start(Tag), gt], !,
	set_line(Line),
	listcontent(Content),
	[Line1],
	[elst].

%element([]), [Line] --> [Line], eos.

% fall back to error if starts like an element, but no previous rule matches
element([]) --> !, % ne rekuru post eraro!
        [Line],
	(
	  lookahead([lbr])
          ;
          lookahead([start(_Tag),gt])
        ),
        set_line(Line),
	call(parse_error_('nevalida elemento')),
	resync_element(_Skipped), !. % ne rekuru post eraro!.

comment(comment(Comment)), [Line1] --> 
	[Line],
	[cstart], 
	set_line(Line),
	comment_text(Comment), 
	[Line1],
	[cend].
%%% ĉu fari tion pli poste...?
% 	{ atomic_list_concat(['<!--'|Cmnt],Cmnt1),
%	atom_concat(Cmnt1,'-->',Comment) }.

comment_text([Text|Rest]) -->
	[Line],
	[ctext(Codes)], !,
	{ atom_codes(Text,Codes) },
	set_line(Line),
	comment_text(Rest).

comment_text(['\n'|Rest]) -->
	[_Line],
	[nl(Line1)], !,
	set_line(Line1),
	comment_text(Rest).

comment_text([]), [Line] --> [Line], lookahead([cend]).

% fall back to error if no other matches
comment_text(Rest) --> !, % ne rekuru post eraro!
	call(parse_error_('nevalida enhavo')), % !, % ne rekuru post kreo de eraro
	resync_content(_Skipped), !, % ne rekuru post resync
        comment_text(Rest).

content([Text|Rest]) -->
	[Line],
	[text(Codes)], !,
	{ atom_codes(Text,Codes) },
	set_line(Line),
	content(Rest).

content([W|Rest]) -->
	[Line],
	[wht(W)], !,
	set_line(Line),
	content(Rest).

content(['\n'|Rest]) -->
	[_Line],
	[nl(Line1)], !,
	set_line(Line1),
	content(Rest).

%content(['~'|Rest]) -->
%	[Line],
%	[tld], !,
%	set_line(Line),
%	content(Rest).
% KOREKTU atributon deduktu el komenclitero antau ~
content([element(tld,[],[])|Rest]) -->
	[Line],
	[tld], !,
        set_line(Line),
        content(Rest).

content([':'|Rest]) -->
	[Line],
	[col], !,
	set_line(Line),
	content(Rest).

content([','|Rest]) -->
	[Line],
	[com], !,
	set_line(Line),
	content(Rest).

content([Element|Rest]) -->
	element(Element), !,
	content(Rest).

content([Comment|Rest]) -->
	comment(Comment), !,
	content(Rest).

content([]), [Line] --> [Line], eos.
%content([]), [Line] --> [Line].
%content([]) --> []. % eos?


% fino de "content" normale estu signita de elementfino, kontrolu tion
% antaŭ eble ĵeti eraron:
content([]), [Line] --> [Line], lookahead([lt]).
content([]), [Line] --> [Line], lookahead([rbr]).


% fall back to error if no other matches
%%%???content(Content), [Line] --> [Line],
content(Rest) --> !, % ne rekuru post eraro!
	call(parse_error_('nevalida enhavo')), % !, % ne rekuru post kreo de eraro
	resync_content(_Skipped), !, % ne rekuru post resync
        content(Rest).



listcontent([W|Rest]) --> % forigu spacsignojn _W en la komenco (?)
%listcontent(Rest) -->
	[Line],
	[wht(W)], !,
	set_line(Line),
        listcontent(Rest).

listcontent(['\n'|Rest]) --> % forigu linirompojn en la komenco (?)
%listcontent(Rest) -->
	[_Line],
	[nl(Line1)], !,
	set_line(Line1),
        listcontent(Rest).

% PLIBONIGU: la scio pri trd/trdgrp estu prefere en "transform" anstataŭ en "parse".
listcontent([element(trdgrp,Lng,Content)|Rest]) -->
	[Line],
	[lng(Lng),col],
	set_line(Line),
	multyitemcontent(Content),!,
        listcontent(Rest).

listcontent([element(trd,Lng,Content)|Rest]) -->
	[Line],
	[lng(Lng),col],
	set_line(Line),
	singleitemcontent(Content),!,
        listcontent(Rest).


% fino de "listcontent" normale estu signita de elementfino, kontrolu tion
% antaŭ eble ĵeti eraron:
listcontent([]), [Line] --> [Line], lookahead([elst]).

% fall back to error if no other matches
listcontent(Rest) --> !, % ne rekuru post eraro!
	call(parse_error_('nevalida listenhavo')),
	resync_listcontent(_Skipped), !, % ne rekuru post eraro!.
        listcontent(Rest).

multyitemcontent([]), [Line] --> [Line], lookahead([lng(_)]),!.
multyitemcontent([]), [Line] --> [Line], lookahead([elst]),!.

multyitemcontent([element(trd,[],Content),','|Rest]) -->
	singleitemcontent(Content),
        [Line],
	[com],
	set_line(Line),
	multyitemcontent(Rest).

multyitemcontent([element(trd,[],Content),',',element(trd,[],Last),'\n']) -->
	singleitemcontent(Content),
        [Line],
	[com],
	set_line(Line),
	singleitemcontent(Last).


singleitemcontent([]), [Line] --> [Line], lookahead([lng(_)]),!.
singleitemcontent([]), [Line] --> [Line], lookahead([com]),!.
singleitemcontent([]), [Line] --> [Line], lookahead([elst]),!.

singleitemcontent([W|Rest]) -->
	[Line],
	[wht(W)], !,
	set_line(Line),
	singleitemcontent(Rest).

singleitemcontent(['\n'|Rest]) -->
	[_Line],
	[nl(Line1)], !,
	set_line(Line1),
	singleitemcontent(Rest).

%listitemcontent([','|Rest]) -->
%	[Line],
%	[com], !,
%	set_line(Line),
%	listitemcontent(Rest).

singleitemcontent([Text|Rest]) -->
	[Line],
	[text(Codes)], !,
	{ atom_codes(Text,Codes) },
	set_line(Line),
	singleitemcontent(Rest).

singleitemcontent(['['|Rest]) -->
	[Line],
	[lsq], !,
	set_line(Line),
	singleitemcontent(Rest).

singleitemcontent([']'|Rest]) -->
	[Line],
	[rsq], !,
	set_line(Line),
	singleitemcontent(Rest).

singleitemcontent([Element|Rest]) -->
	element(Element), !,
	singleitemcontent(Rest).

% fall back to error if no other matches
singleitemcontent(Rest) --> !, % ne rekuru post eraro!
	call(parse_error_('nevalida listero')),
	resync_listcontent(_Skipped), !, % ne rekuru post eraro!.
        listcontent(Rest).

parse_error_(Msg,[Line|Rest],[Line|Rest]) :-
  once(( % ne kreu saman eraron diversmaniere...
	first_part_of_list(20,Rest,Begin),
% debugging...
%%%%	output_error(Line,Msg,Begin),
	assertz(vrt_parse_error(Line,Msg,Begin))
  )).
%  throw(invalid_content_at(Line,Begin)).


resync_element([One|Skipped]), [Line1] -->
	[Line],
	[One],
	string_without([nl(_),lt,rbr],Skipped),
        ([rbr], {Line1=Line} ; [lt,end(_Tag)], {Line1=Line} ; [nl(Line1)]).


/* *
resync_content([One|Skipped]), [Line] -->
	[Line],
	[One], string_without([nl(_),lt,rbr],Skipped).
* */

resync_content([One]), [Line] -->
	[Line],
	[One].

resync_listcontent([One|Skipped]), [Line] -->
	[Line],
	[One], string_without([nl(_),lng(_),elst],Skipped).


/* *
% eble tio estas nenecesa... aŭ devus esti en resync_element anstataŭe
resync_content([Element|Rest]) -->
	[Line],
	string_without([rbr],_),
	set_line(Line),
	element(Element),
	content(Rest).

% eble tio estas nenecesa... aŭ devus esti en resync_element anstataŭe
resync_content([Element|Rest]) -->
	[Line],
	string_without([lt],_),
	lt,
	tag(_),
	set_line(Line),
	element(Element),
	content(Rest).
* */

