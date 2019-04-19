/* -*- Mode: Prolog -*- */
:- module(vrt,[
	      vrt/2
	  ]).

/***
reguloj (DCG) por krei tekston en formato VRT el arbostrukturo
(AST, abstrakta sintaksarbo)
***/


vrt(Content,Output) :-
  Indent = [],
  phrase(vrt(Indent,Content),Output),!.

vrt_attr(Attr,Attr) :- atomic(Attr).
vrt_attr([],'').
vrt_attr(Attrs,Atom) :-
  is_list(Attrs),
  atomic_list_concat(Attrs,',',Atom).


vrt(_,[]) --> [].

% listo de elementoj kaj tekstoj

vrt(Indent,[First|Rest]) -->
  vrt(Indent,First), !,
  handle_newline([First|Rest]),
  handle_indent(Indent,[First|Rest]),
  vrt(Indent,Rest).

% elemento kun sia enhavo

vrt(Indent,blk(Elem,Attr,Content)) -->
    tag(Elem), attr(Attr), ">", nl,
    indent([32,32|Indent]),
    vrt([32,32|Indent],Content),
    nl, indent(Indent),
    "<", tag(Elem), !.

vrt(Indent,lst(Elem,Attr,Content)) -->
  "[[", tag(Elem), attr(Attr), ">", nl,
  indent([32,32|Indent]),
  vrt([32,32|Indent],Content),
  nl, indent(Indent),
  %% "<", tag(Elem),
  "]]",!.


% tradukoj
vrt(Indent,lin(trd,Attr,Content)) -->
    text(Attr), ": ",!,
    vrt([32,32|Indent],Content).

vrt(Indent,lin(trdgrp,Attr,Content)) -->
    text(Attr), ": ",!,
    vrt([32,32|Indent],Content).


% aliaj "line" elementoj
vrt(Indent,lin(Elem,Attr,Content)) -->
    tag(Elem), attr(Attr), ">",!,
    vrt([32,32|Indent],Content),
    "<", tag(Elem).

% tildo
vrt(_Indent,inl(tld,[],_)) --> "~".
vrt(_Indent,inl(tld,Attr,_)) --> text(Attr), "~".

% traduko ene de trdgrp -> KOREKTU tio influas
% ankau ene de dif, do necesas trakti tion jam en trf
vrt(Indent,inl(trd,[],Content)) -->
    vrt([32,32|Indent],Content).


% other "inline" elements
vrt(Indent,inl(Elem,Attr,Content)) -->
    "{", tag(Elem), attr(Attr), ">",
    vrt([32,32|Indent],Content),
    "}".

vrt(Indent,var(Elem,Attr,Content)) -->
  {
    type(var(Elem,Attr,Content),Type),
    Item =.. [Type,Elem,Attr,Content]
  } ->
  vrt(Indent,Item).

vrt(_Indent,comment(Comment)) -->
  "<!--",
  text(Comment),
  "-->".

% tekstoj

vrt(_Indent,Text) -->
  { atomic(Text) }
  ->
    text(Text),!.


% eraro en eligo de elemento

vrt(_,element(Elem,_,_)) --> [],
  { throw(elemento_ne_difinita(Elem)) }.


%%%%%%%%%% bazaj gramatikaj reguloj

tag(Elem) --> { atom_codes(Elem,SElem) }, SElem.

text(Atom) --> { atom(Atom), !, atom_codes(Atom,Text) },  Text.
text(Text) --> Text.

indent(Indent) --> Indent.

nl --> "\n".
eq --> "=".

%%%% atributoj

attr([]) --> [].
attr(Attrs) --> ":", attr_(Attrs).

%attr_([]) --> [].
attr_(Atom) --> { atom(Atom) } -> text(Atom).
attr_(Attr=Val) --> text(Attr), eq, text(Val).
%attr_([Attr]) --> text(Attr).
attr_([Attr]) --> attr_(Attr).
attr_([Attr,Next|Rest]) -->
    text(Attr), ",",
    attr_([Next|Rest]).


%%% logiko por novaj linioj kaj ensovoj...

newline(any,blk).
newline(blk,any).
%newline(blk,none).
newline(any,lst).
newline(lin,any).
newline(any,lin).


%indent(dummy,dummy).

indent(any,blk).
indent(blk,text).
indent(blk,inl).

indent(any,lst).
%indent(lst,text).
%indent(lst,inl).

indent(any,lin).
%indent(lin,inl).
indent(lin,any).

indent(none,text).
indent(none,inl).
%indent(none,lin).


% ne aldonu linirompon post linirompo
handle_newline(['\n'|_]) --> !.

% aldonu linirompon inter du eroj lau ilia tipo
% kaj la supraj reguloj newline(Unua,Dua).
handle_newline([Item|Rest]) -->
  {
    type(Item,T1),
    type(Rest,T2),
    newline(T1,T2),
    !
  },
  nl.

handle_newline(_) --> []. % faru nenion


handle_indent(Indent,[Item|Rest]) -->
  {
    type(Item,T1),
    type(Rest,T2),
    indent(T1,T2),
    !
  },
  indent(Indent).

handle_indent(_Indent,_) --> []. % faru nenion

text_only([]).
text_only([Content|Rest]) :-
	type(Content,text),
	text_only(Rest).

%type(Elem,Content,Type) :-
%  type(element(Elem,_,Content),Type).

type([Item|_],Type) :- type(Item,Type).

type(Item,text) :- atomic(Item).

type(Item,Type) :-
  Item =.. [Type,_,_,_],
  memberchk(Type,[blk,lst,lin,inl]). % ekz. blk(Elem,Atr,Content) -> blk

% eble traktu var jam en transform ?
type(var(_,_,Content),inl) :- text_only(Content).
type(var(_,_,Content),blk) :- \+ text_only(Content).

type([],none).
type(C,any) :- C \= [].





