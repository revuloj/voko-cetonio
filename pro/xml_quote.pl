/* -*- Mode: Prolog -*- */
:- module(xml_quote,[
	prepare_entity_index/0,
	reverse_entity_index/2,
	check_unique_entity_values/4,
	entity_pairs/2,
        entity_value_length_index/2,
	entity_value_1_index/2,
	get_entity_index/3,
	dom_cnodes/2,
	xml_quote_cdata/5,
	xml_unquote_cdata/2
    ]).

:- use_module(library(dcg/basics)).
%:- dynamic entity/1.
:- use_module(entities).
:- encoding(utf8).
:- use_module(spacoj).



test(1,'Eĥoŝanĝo ĉiuĵaŭde').

test(2,'Hans Christian Andersen, trad. Zamenhof kaj aliaj verkoj').

test(3,'<Άμστερνταμ.>&#34;"?"').

user:portray(Assoc) :-
  is_assoc(Assoc),
  write('<assoc>').

test_quote(N,Quoted) :-
  test(N,Atom),
  reverse_entity_index(entity,XReverse),
  entity_value_length_index(entity,XLens),
  xml_quote_cdata(Atom,Quoted,XReverse,XLens,utf8).

prepare_entity_index :-
    reverse_entity_index(entity,ReverseEntInx),
    entity_value_length_index(entity,EntValLenInx),
    entity_value_1_index(entity,EntVal1Inx),
    nb_setval(xreverse,ReverseEntInx),
    nb_setval(xlengths,EntValLenInx),
    nb_setval(x1entval,EntVal1Inx).

get_entity_index(ReverseEntInx,EntValLenInx,EntVal1Inx) :-
    nb_getval(xreverse,ReverseEntInx),
    nb_getval(xlengths,EntValLenInx),
    nb_getval(x1entval,EntVal1Inx).


user:sgml_write_node_hook(comment(C),Out,_State) :-
  sub_atom(C,0,4,_,'<!--') 
    ->  write(Out,C)
    ;
    write(Out,'<!--'), write(Out,C), write(Out,'-->').

user:sgml_write_node_hook(content(Content),Out,_State) :-
  atomic(Content),
  nb_getval(xreverse,ReverseEntInx),
  nb_getval(xlengths,EntValLenInx),
  xml_quote_cdata(Content,Quoted,ReverseEntInx,EntValLenInx,utf8),
  write(Out,Quoted).


% anstatŭigu unuojn per litervicoj
xml_unquote_cdata([],[]).

xml_unquote_cdata([0'&|In],Quoted) :- !,
    %nth0(N,In,0';),
    %length(Entity,N),
    append(Entity,[0';|Rest],In),
    once((
	unquote_char(Value,Entity)
	;
	atom_codes(Ent,Entity),
	\+ memberchk(Ent,[amp,apos,lt,gt,quot]), % ne traduku HTML-unuojn	
	entity(Ent,Val),
	atom_length(Val,1), % traduku nur unuliterajn unuojn
	atom_codes(Val,Value)
	;
	append([0'&|Entity],[0';],Value)
    )),
    xml_unquote_cdata(Rest,QuotedRest),!,
    append(Value,QuotedRest,Quoted).

xml_unquote_cdata([F|In],[F|Quoted]) :-
    xml_unquote_cdata(In,Quoted).


unquote_char([Chr],[0'#,0'x|Hex]) :- !,
    phrase(xinteger(Chr),Hex).

unquote_char([Chr],[0'#|Int]) :-
    phrase(integer(Chr),Int).

xml_quote_cdata([],[],_,_,_).
xml_quote_cdata([First|In],Quoted,ReverseEntInx,EntValLenInx,Encoding) :-
    once((
	% trovu ĉiujn longecojn de unuoj kun tiu komenclitero
	char_code(Letter,First),
	get_assoc(Letter,EntValLenInx,Lengths),
	% provu anstataŭigi per unuo en la komenco
	quote_at_start([First|In],QuHead,Tail,Lengths,ReverseEntInx,Encoding)
	;
	quote_char_code(First,QuHead),
	Tail = In
    )),

    % daŭrigu per la resto
    xml_quote_cdata(Tail,QuTail,ReverseEntInx,EntValLenInx,Encoding),
    append(QuHead,QuTail,Quoted).

quote_at_start(In,QuHead,Tail,[Len|Lengths],ReverseEntInx,Encoding) :-
    once((
	length(Head,Len),
	append(Head,Tail,In),
	atom_codes(HeadA,Head),
	get_assoc(HeadA,ReverseEntInx,Entity),
	atom_codes(Entity,EntityC),
  % kunmetu &+EntityC+;
	append([0'&|EntityC],[0';],QuHead)
	;
	quote_at_start(In,QuHead,Tail,Lengths,ReverseEntInx,Encoding)
    )).
  
quote_at_start([First|Tail],QuHead,Tail,[],_,_) :-
    quote_char_code(First,QuHead).

quote_char_code(Code,[Code]) :-
    Code=<255,!.

quote_char_code(Code,Entity) :-
    Code>255,
    format(codes(Entity),'&#~d;',[Code]).


/** malrapida pro uzo de atomoj anstataŭ listoj
% antataŭigu litervicojn per unuoj (&...;)
xml_quote_cdata(In,Quoted,ReverseEntInx,EntValLenInx,Encoding) :-
  sub_atom(In,0,1,_,First),
  once((
    % trovu ĉiujn longecojn de unuoj kun tiu komenclitero
    get_assoc(First,EntValLenInx,Lengths),
    % provu anstataŭigi per unu en la komenco
    quote_at_start(In,QuHead,Len,Lengths,ReverseEntInx,Encoding),
    sub_atom(In,Len,_,0,Tail)
    ;
    quote_char(First,QuHead),
    Len = 1
  )),
  sub_atom(In,Len,_,0,Tail),
  % daŭrigu per la resto
  xml_quote_cdata(Tail,QuTail,ReverseEntInx,EntValLenInx,Encoding),
  atom_concat(QuHead,QuTail,Quoted).

xml_quote_cdata('','',_,_,_). 

% longevoj estas ordigitaj, do unu provu anstataŭigi longajn signovicojn per unuo
quote_at_start(In,QuHead,Len1,[Len|Lengths],ReverseEntInx,Encoding) :-
  once((
    sub_atom(In,0,Len,_,Head),
    get_assoc(Head,ReverseEntInx,Entity),
    atomic_list_concat(['&',Entity,';'],QuHead),
    Len1 = Len
    ;
    quote_at_start(In,QuHead,Len1,Lengths,ReverseEntInx,Encoding)
  )).
  
quote_at_start(In,QuHead,1,[],_,_) :-
  sub_atom(In,0,1,_,First),
  quote_char(First,QuHead).

***/

quote_char('&','&amp;'):-!.
quote_char('''','&apos;'):-!.
quote_char('<','&lt;'):-!.
quote_char('>','&gt;'):-!.
quote_char('"','&quot;'):-!.

quote_char(Chr,Chr) :-
  char_code(Chr,Code),
  Code=<255,!.

quote_char(Chr,Entity) :-
  char_code(Chr,Code),
  Code>255,
  format(atom(Entity),'&#~d;',[Code]).


entity_pairs(Goal,Pairs) :-
  setof(
    Name-Value,
    N^V^(
	call(Goal,N,V),
	atom_codes(N,Name),
	atom_codes(V,Value)
    ),
    Pairs
  ).

%! reverse_entity_index(+Goal,-Index) is det.
%
% Kreas indekson por inversa serĉo de XML-unuo-nomoj per ties valoro.
% =Goal= redonas la erojn indeksendajn, en nia kazo tio estas la faktoj entity/2.

reverse_entity_index(Goal,Index) :-
  \+ (
    check_unique_entity_values(Goal,N1,N2,V),
    format('ERROR entity value not unique ~s ~s ~s~n',[N1,N2,V])
  ),
  setof(Value-Key,call(Goal,Key,Value),Items),
  ord_list_to_assoc(Items,Index).


%! entity_value_length_index(+Goal,-LengthIndex) is det.
%
% e.g. =Goal= = =entity= for =entity(Key,Value)=
% Kreas indekson de unuaj literoj de unuoj kaj ordigitaj longecoj de ĉiuj ili
% ekz. [a-[5,2,1],c-[9,4,2,1]|_]
entity_value_length_index(Goal,LengthIndex) :-
  setof(
    Letter-Len,
    value_letter_length(Goal,Letter,Len),
    LetterLens
  ),
  group_pairs_by_key(LetterLens,LetterLensGrouped),
  maplist(sort_lengths,LetterLensGrouped,LetterLensSorted),
  list_to_assoc(LetterLensSorted,LengthIndex).


%! entity_value_1_index(+Goal,-Index) is det.
%
% Kreas indekson de literaj por kiuj ekzistas unuo, sama
% strukturo kiel ĉe =entity_value_length_index=, por povi 
% uzi la saman predikaton por anstataŭigado
% ekz. [a-[1],c-[1]|_]
entity_value_1_index(Goal,Index) :-
  setof(
    Letter-[1],
    value_letter(Goal,Letter),
    LetterLens
  ),
%  group_pairs_by_key(LetterLens,LetterLensGrouped),
%  maplist(sort_lengths,LetterLensGrouped,LetterLensSorted),
  list_to_assoc(LetterLens,Index).

value_letter(Goal,Letter) :-
    call(Goal,Key,Value),
    \+ memberchk(Key,[lt,gt,apos,quot,amp]),
    sub_atom(Value,0,1,_,Letter).


value_letter_length(Goal,Letter,Length) :-
  call(Goal,_Key,Value),
  sub_atom(Value,0,1,_,Letter),
  atom_length(Value,Length).

sort_lengths(Letter-LengthList,Letter-Sorted) :- sort(0,@>,LengthList,Sorted).

check_unique_entity_values(Goal,N1,N2,V) :-
  call(Goal,N1,V),
  call(Goal,N2,V),
  N1 @> N2.

dom_cnodes(element(Tag,Attr,Cnt),element(Tag,Attr,CNCnt)) :-
  dom_cnodes(Cnt,CNCnt).

dom_cnodes([element(Tag,Attr,Cnt)|Tail],[element(Tag,Attr,CNCnt)|CNTail]) :- !,
  dom_cnodes(Cnt,CNCnt),
  dom_cnodes(Tail,CNTail).

dom_cnodes([comment(Cmnt)|Tail],[comment(Cmnt)|CNTail]) :- !,
  dom_cnodes(Tail,CNTail).

% ne transformu simplan enhavon, kie
% anstataŭigo de unuoj (entities) ne okazos
dom_cnodes(['\n'|Tail],['\n'|CNTail]) :- !,
  dom_cnodes(Tail,CNTail).
dom_cnodes([' '|Tail],[' '|CNTail]) :- !,
  dom_cnodes(Tail,CNTail).
dom_cnodes([Spaces|Tail],[Spaces|CNTail]) :- 
  atomic(Spaces),
  spacoj:spaces(Spaces), !,
  dom_cnodes(Tail,CNTail).

% kreu nodojn content (Atomic)
% por aparte trakti ties enhavon dum XML-skribado post,
% t.e. anstatŭigi unuojn (entities)
dom_cnodes([Atomic|Tail],[content(Atomic)|CNTail]) :-
  atomic(Atomic),
  dom_cnodes(Tail,CNTail).

dom_cnodes([],[]).

:- thread_initialization prepare_entity_index.
