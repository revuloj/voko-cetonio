/* -*- Mode: Prolog -*- */
:- module(transform,[
	      trf_list/2
	  ]).

:- use_module(spacoj).
:- consult(transform_map).
:- dynamic trf_map:art_mrk/1.

/***
Reguloj por transformi strukturarbon de XML (DOM) al
strukturarbo por generi formaton VRT kaj inversdirekte:
kelkaj reguloj funkcias ambaŭdirekte, aliaj estas markitaj per
x2v ... v2x por montri la direkton
***/

trf_list(VList,xml(_,XList)) :- trf_list('',VList,XList,'').

%% trf_list(VParent,VList,XList,XParent).

trf_list(_,[],[],_).

% speciala kazo por kolektitaj elementoj (trdj)
trf_list(VParent,[lst(VElem,[],VContent)|VTail],[XFirst|XRest],XParent) :-
  var(VElem), % de XML al Vrt
  % ĉu temas pri elemento kolektenda en listo?
  XFirst = element(XElem,_,_),
  aggr_map(XElem,XParList,VElem),
  memberchk(XParent,XParList),
  % se jes kolektu la elementojn en kadra elemento
  % ĉu tiel anstatŭigi VParent per lst(VElem...)?
  trf_aggr_x2v(VElem,VContent,[XFirst|XRest],XTail,XParent),
  trf_list(VParent,VTail,XTail,XParent).

trf_list(VParent,[lst(VElem,[],VContent)|VTail],XContent,XParent) :-
  nonvar(VElem), % de Vrt al XML
  % ĉu temas pri elemento kolektenda en listo?
  % VContent = [element(VElem,_,_)|_],
  aggr_map(_XElem,_XParList,VElem),
  % XParent = element(XPar,_,_),
  % memberchk(XPar,XParList),
  % se jes kolektu la elementojn en kadra elemento
  % ĉu tiel anstatŭigi VParent per lst(VElem...)?
  trf_aggr_v2x(VElem,VContent,XCnt,'',XParent),
  append(XCnt,XTail,XContent),
  trf_list(VParent,VTail,XTail,XParent).


% normala kazo: trakuru la liston transformante ĉiun eron
trf_list(VParent,[VFirst|VRest],[XFirst|XRest],XParent) :-
   trf_item(VParent,VFirst,XFirst,XParent),
   trf_list(VParent,VRest,XRest,XParent).


% transformi la DOM-arbon inter la Xml-konvena strukturo kaj la koncizigita Vrt-strukturo

% transformo de elemento kaj atributoj
trf_item(_VParent,VElement,XElement,XParent) :-
  var(VElement) 
  -> % de XML al Vrt
    trf_item_x2v(_,VElement,XElement,XParent).

trf_item(_VParent,VElement,XElement,_XParent) :-
  var(XElement) 
  -> % de Vrt al XML
    trf_item_v2x(_,VElement,XElement,_).

% unupartan komenton transformu idente, liston kunigu
trf_item(_,comment(VCmt),comment(XCmt),_) :- string_codes(VCmt,XCmt). % atomic(Cmt).
trf_item(_,comment(CmtLst),comment(Cmt),_) :- 
	is_list(CmtLst),
        atomic_list_concat(CmtLst,Cmt).

trf_item(_,String,pcdata(Text),_) :- string_codes(String,Text). % atomic(Text).
trf_item(_,'',doctype(vortaro,_),_). % ignoru doctype provizore, eble uzu pli lerte por enkonduka linio en Vrt

% tekston transformu al identa teksto
trf_item(_,Text,Text,_) :- atomic(Text).
%%trf_item(_,Text,content(Text),_) :- atomic(Text). % necesa por trakti XML-enhavon aprte dum xml_write...
%%trf_item(_,Text,Text,_) :- atomic(Text).
%trf_item(_,[Sign],[Sign],_) :- delimiters(Delim), memberchk(Sign,Delim).

% ĵetu eraron, se neniu el la  antaŭaj reguloj konvenis
trf_item(Vp,Vi,Xi,_Xp) :- var(Xi), throw(trf_item_error(Vp/Vi)),!.
trf_item(_Vp,Vi,Xi,Xp) :- var(Vi), throw(trf_item_error(Xp/Xi)),!.


trf_item_x2v(_VParent,VElement,XElement,XParent) :-
  XElement =.. [element,XElem,XAttr,XContent], % temas pri elemento
  trf_eltyp(VType,XElem,XParent),
  VElement =.. [VType,VElem,AttrVal,VContent],
  attrlst_atom_codes(AttrLst,XAttr),
  trf_elat(VElem,XElem,AttrVal,AttrLst),
%  trimx(XContent,NContent),
  normalizex(XContent,NContent),
  % XContent = TrimmedContent,
  trf_list(VElem,VContent,NContent,XElem).

trf_item_v2x(_VParent,VElement,XElement,_XParent) :-
  VElement =.. [_VType,VElem,AttrVal,VContent],
  trf_elat(VElem,XElem,AttrVal,XAttr),
  XElement =.. [element,XElem,XAttr,XContent],
  %trf_eltyp(VType,XElem,XParent),
  trf_list(VElem,VContent,XContent,XElem).

attrlst_atom_codes(AttrLst,XAttrLst) :-
    maplist(attr_atom_codes,AttrLst,XAttrLst).

attr_atom_codes(AttrName=AttrVal,AttrName=XAttrVal) :- atom_codes(AttrVal,XAttrVal).


% transformado de aggregitaj listoj [[<trd-oj>]]

trf_aggr_x2v(VElem,[VFirst|VRest],[XFirst|XRest],XTail,XParent) :-
  % ĉu temas pri elemento kolektenda en listo?
  XFirst = element(XElem,_,_),
  aggr_map(XElem,_,_), % ne plu necesas kontroli la parencon, tion faris la vokanto (trf_list)!
  %
  trf_item(VElem,VFirst,XFirst,XParent),
  trf_aggr_x2v(VElem,VRest,XRest,XTail,XParent).

% se la listo ne plu enhavas kolektendan elementon redonu reston kiel XTail
trf_aggr_x2v(_,[],XTail,XTail,_).


trf_aggr_v2x(VParent,[VFirst|VRest],XContent,XIndent,XParent) :-
  trf_item(VParent,VFirst,XItem,XParent),
  indent(XItem,XIndent1,XIndent), % provu registri deŝovojn (spacojn) en la enhavo
                                  % PLIBONIGENDA: akceptu nur spacojn post linirompoj '\n'
  (XItem =.. [element|_] 
   ->	trimv(XItem,XTrimmed),   %% forigas tro...!
	% trimv2(XItem,XTrimmed),   %% forigas maltro...!
        %normalizex(XItem,XTrimmed), %%%?
        fold(XTrimmed,XFirst,XIndent1),
	trf_aggr_v2x(VParent,VRest,XRest,XIndent1,XParent),
        decr_indent(XIndent1,XIndent2), % iom kruda solvo, XIndent1 normale havas du spacojn tro pro [[trdj
	XContent = [XFirst,'\n',XIndent2|XRest]
    ;
	trf_aggr_v2x(VParent,VRest,XRest,XIndent1,XParent),
	(XItem = Spaces, spacoj:spaces(Spaces)
         ->  
	   XContent = XRest
         ;
	   XContent = [XItem|XRest]
        )
    ).
 

trf_aggr_v2x(_,[],[],_,_).


% transformo de elementtipo
trf_eltyp(VType,XElem,XParent) :-
  once((
    eltp_map(XParent/XElem,VType)
    ;
    eltp_map(XElem,VType)
  )).


% transformo de unuopa elemento sen atributo
%trf_elat(Elem,Elem,[],[]).

% transformo de senatributa elemento kun fiksita atributlisto
trf_elat(VElem,XElem,[],XAttrVal) :-
%  atom_codes(AttrVal,XAttrVal),  
  elat_map(VElem,XElem,_,XAttrVal),!.

% transformo de unuopa elemento sen atributo
trf_elat(Elem,Elem,[],[]).

% transformo de elementonomoj kaj atributo(j), laŭ pli komplika
% transformado
trf_elat(VElem,XElem,VAttrVals,XAttrListPlena) :-
  var(VElem),
  elat_map(VElem,XElem,XAttrs,XFiksAttrList),
  disigo(XAttrListPlena,XFiksAttrList,XAttrListRest),
  trf_attr_x2v(XElem,VAttrVals,XAttrs,XAttrListRest).

trf_elat(VElem,XElem,VAttrVals,XAttrListPlena) :-
  var(XElem),
  elat_map(VElem,XElem,XAttrs,XFiksAttrList),
  atomic_list_concat(VAttrValList,',',VAttrVals),
  trf_attr_v2x(XElem,VAttrValList,XAttrs,XAttrList),
  append(XFiksAttrList,XAttrList,XAttrListPlena).


% transformo de unuopa atributo depende de elementnomo
trf_elat(Elem,Elem,AttrVal,[XAttr=AttrVal]) :-
    attr_map(Elem,XAttr),!.

trf_elat(Elem,Elem,VAttrVal,[XAttr=XAttrVal]) :-
  attr_map(Elem,XAttr,VAttrVal,XAttrVal),!.


disigo(XAttrListPlena,XFiksAttrList,XAttrListRest) :-
  partition(membro(XFiksAttrList),XAttrListPlena,Included,XAttrListRest),
  % kontrolu ĉu Included entenas samajn membrojn kiel XFiksAttrList
  exclude(membro(Included),XFiksAttrList,[]).

membro(List,X) :- memberchk(X,List).

% transformado de atributoj
% la ordo de atributoj estas fiksita en la Vrt-strukturo, sed libera en la Xml-strukturo
% fiksitaj atributoj estas ignorataj dum la transformado tie ĉi
%trf_attr(_,[],_,[]).
%trf_attr(XElem,VAttr,XAttr,XAttrList) :-
%  var(VAttr),
%  trf_attr_x2v(XElem,VAttr,XAttr,XAttrList).

%trf_attr(XElem,VAttr,XAttr,XAttrList) :-
%  var(XAttrList),
%  trf_attr_v2x(XElem,VAttr,XAttr,XAttrList).

% transformado de atributoj de Vrt al Xml
trf_attr_v2x(_,[],[],[]).
trf_attr_v2x(XElem,[VAttrVal|VRest],[XAttr|XRest],[XAttr=XAttrVal|Rest]) :-
  attr_map(XElem,XAttr,VAttrVal,XAttrVal),
  trf_attr_v2x(XElem,VRest,XRest,Rest).

% atributojn laŭ la formo Attr=Val se registritaj sennomaj atributoj jam
% estas konsumitaj (Xattr=[]), transprenu rekte
trf_attr_v2x(XElem,[VAttr_Val|VRest],[],[Attr=Val|Rest]) :-
  atomic_list_concat([Attr,Val],'=',VAttr_Val),
  trf_attr_v2x(XElem,VRest,[],Rest).

% estas eraro se atributoj restas
trf_attr_v2x(XElem,VRest,[],_) :- VRest \= [],
  throw(spare_atribute(XElem,VRest)).


% transformado de atributoj de Xml al Vrt
trf_attr_x2v(_,[],[],[]).
trf_attr_x2v(XElem,[VAttrVal|VRest],[XAttr|XRest],XAttrList) :-
  % trovu XAttr en la listo kaj metu la valoron al VAttrList
  member(XAttr=XAttrVal,XAttrList),
  attr_map(XElem,XAttr,VAttrVal,XAttrVal),
  % forigu la trovitan el la listo, por certigi, ke fine la listo estu maplena
  exclude(membro([XAttr=XAttrVal]),XAttrList,XAttrListRest),
  % traktu la ceteran
  trf_attr_x2v(XElem,VRest,XRest,XAttrListRest).

% alpendigu restantajn atributojn kiel nomo=valoro
trf_attr_x2v(_,AttrRest,[],AttrRest). 

% estas eraro, se XAttrList ne estas malplena fine
%trf_attr_x2v(XElem,[],[],XAttrList) :- XAttrList \= [], 
%	throw(netraktitaj_atributoj(XElem,XAttrList)).
/**
trimx([Text|Content],TrimmedContent) :-
  atomic(Text), 
  spacoj:trim_left(Text,Trimmed),
  (Trimmed = '' 
    -> trimx(Content,TrimmedContent)
    ; 
    normalizex([Trimmed|Content],TrimmedContent)
  ).
**/

%
%  laŭ XML-normo:
%  - forigu spacojn antaŭ unua kaj post lasta elemento
%  - anstataŭigu sinsekvajn spacsignojn per unu spaco ' '

% forigu spacojn ĉe la komenco
normalizex([Spaces,Element|Content],[Element|NContent]) :-
  atomic(Spaces), 
  compound(Element),
  spaces(Spaces),
  normalizex(Content,NContent).

% enŝovu linion ĉe certaj elementoj
normalizex([Element|Content],['\n',Element|NContent]) :-
  Element =.. [element,Tag|_],
  el_line_before(Tag),
  normalizex(Content,NContent).
	
% forigu spacojn ĉe la fino
normalizex([Element,Spaces],[Element]) :-
  compound(Element),
  atomic(Spaces),
  spaces(Spaces),!.

% forigu troajn spacojn ene de teksto
normalizex([Text|Content],[NText|NContent]) :-
  atomic(Text),!,
  spacoj:normalize(Text,NText),
  normalizex(Content,NContent).

normalizex([NonAtomic|Content],[NonAtomic|NContent]) :-
  normalizex(Content,NContent).

normalizex([],[]).

/***

trimv2(element(Tag,Attr,Content),element(Tag,Attr,TrimmedCnt)):-
  normalizev(Content,TrimmedCnt).

% forigu spacojn ĉe la komenco
normalizev([Spaces,Element|Content],[Element|NContent]) :-
  atomic(Spaces), 
  Element =.. [element|_],
  spaces(Spaces),
  normalizev(Content,NContent).

% forigu spacojn ĉe la fino
normalizev([Element,Spaces],[Element]) :-
  Element =.. [element|_],
  atomic(Spaces),
  spaces(Spaces),!.

% forigu troajn spacojn ene de teksto
normalizev([Text|Content],[NText|NContent]) :-
  atomic(Text),!,
  spacoj:normalize(Text,NText),
  normalizev(Content,NContent).

normalizev([NonAtomic|Content],[NonAtomic|NContent]) :-
  normalizev(Content,NContent).

normalizev([],[]).

***/

trimv(element(Tag,Attr,Content),element(Tag,Attr,TrimmedCnt)):-
  el_trim(Tag),
  once(trimleft(Content,TrimmedLeft)),
  once(trimright(TrimmedLeft,TrimmedCnt)).

trimv(X,X). % apliku nur al elementoj

trimleft([Whites|Content],TrimmedContent):-
  whites(Whites),
  trimleft(Content,TrimmedContent).

trimleft(['\n'|Content],TrimmedContent):-
  trimleft(Content,TrimmedContent).

%%  tio forigas tro, au testu la enhavon antaue...?
trimleft([element(Tag,Attr,Content)|Rest],[element(Tag,Attr,TrimmedContent)|TrimmedRest]):-
  el_trim(Tag),
  trimleft(Content,TrimmedLeft),
  trimright(TrimmedLeft,TrimmedContent),
  trimleft(Rest,TrimmedRest).

trimleft(Content,Content).

trimright(Content,TrimmedContent):-
  reverse(Content,Reversed),
  trimleft(Reversed,TrimmedReversed),
  reverse(TrimmedContent,TrimmedReversed),!.

% aldonu linirompon+deshovon inter elementoj, ekz. trd en trdgrp...
fold(element(Tag,Attr,Content),element(Tag,Attr,FoldedContent),Indent) :-
	el_fold(Tag/_), % elemento estas parenco de faldenda, ekz. trdgrp/_
	fold_(Tag,Content,FoldedCnt,Indent), % faldu la enhavon
	atom_concat(Ind1,'  ',Indent),
	append(FoldedCnt,['\n',Ind1],FoldedContent). % aldonu linirompon+deshovon antaŭ finmarko, 
                                                     % ekz '\n   </trdgrp>'

/*
fold(element(Tag,Attr,Content),element(Tag,Attr,FoldedContent),Indent) :-
	el_fold(Tag), % elemento estas faldenda, ekz. trd
	fold_(Tag,Content,FoldedCnt,Indent), % faldu la enhavon
	atom_concat(Ind1,'  ',Indent),
	append(FoldedCnt,['\n',Ind1],FoldedContent). % aldonu linirompon+deshovon post finmarko, 
*/

fold(X,X,_).

% faldado de la enhavo, ekz. de trdgrp

% kazo: faldenda elemento post linirompo (trdgrp/trd)
fold_(XParent, % enshovu Indent antau la elemento
      ['\n',element(Tag,Attr,Content)|Rest],
      ['\n',Indent,element(Tag,Attr,IndentedContent)|IndentedRest],
      Indent):-
  el_fold(XParent/Tag), % elemento estas faldenda, ekz. trdgrp/trd
  fold_(Tag,Content,IndentedContent,Indent),
  fold_(XParent,Rest,IndentedRest,Indent).

% kazo: faldenda elemento post linirompo (trdgrp/trd) sen antaŭa linirompo
fold_(XParent,
      [element(Tag,Attr,Content)|Rest],
      ['\n',Indent,element(Tag,Attr,IndentedContent)|IndentedRest],
      Indent):-
  el_fold(XParent/Tag),  % elemento estas faldenda, ekz. trdgrp/trd
  fold_(Tag,Content,IndentedContent,Indent),
  fold_(XParent,Rest,IndentedRest,Indent).


% kazo alia elemento, ekz klr, ind ene de trd
fold_(XParent,
      [element(Tag,Attr,Content)|Rest],
      [element(Tag,Attr,IndentedContent)|IndentedRest],
      Indent):-
  fold_(Tag,Content,IndentedContent,Indent),
  fold_(XParent,Rest,IndentedRest,Indent).

% kazo: ne-elementa enhavo, do tekstoj...
fold_(XParent,[Any|Content],[Any|IndentedContent],Indent) :-
  fold_(XParent,Content,IndentedContent,Indent).

fold_(_,[],[],_).


indent(Item,Item,_) :- whites(Item),!.
indent(_,Indent,Indent).

decr_indent(Indent,DecrInd) :-
  atom_length(Indent,L),
  L>1, L2 is L-2, 
  sub_atom(Indent,0,L2,_,DecrInd).
decr_indent(Ind,Ind).


