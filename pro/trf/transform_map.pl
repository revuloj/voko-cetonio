/* -*- Mode: Prolog -*- */
:- module(trf_map,[
	      attr_map/2,
	      attr_map/4,
	      elat_map/4,
	      aggr_map/3,
	      eltp_map/2,
	      el_fold/1,
	      el_trim/1,
	      el_line_before/1
       ]).

/***
Konkretaj reguloj uzataj de transform.pl difinantoj kiel elementoj laŭ 
vokoxml.dtd transformigu en formaton VRT kaj inverse
***/

%% attr_map(Elem:atom, XmlAttr:atom) is nondet.
%% attr_map(Elem:atom, XmlAttr:atom, VrtAttrVal:atom, XmlAttrVal:atom
%

% se elemento en Vrt-formato havas unu sennoman atributon
% ĝi transformiĝas al tiu nomita tie ĉi kiel dua argumento
attr_map(klr,tip).
attr_map(url,ref).

attr_map(mlg,kod).

attr_map(tld,lit). % ĉu tuj transformi al teksto...? verŝajne ne bone!


% provizore transformu unuope,
%TODO: poste kolektu ĉiujn tradukojn en unu listo
attr_map(trd,lng).
attr_map(trdgrp,lng).

% transformi markojn

attr_map(art,mrk,VMrk,XMrk) :- var(VMrk), VMrk = XMrk.
%attr_map(art,mrk,VMrk,XMrk) :- var(VMrk),
%	atomic_list_concat([_,File|_],' ',XMrk),
%	atom_concat(VMrk,'.xml,v',File).

attr_map(art,mrk,VMrk,XMrk) :- nonvar(VMrk),
	XMrk = VMrk,
	% marko enhavas la dosieronomon, kiun ni
	% bezonos por prefiksi al sekvontaj markoj
	atomic_list_concat([_,File|_],' ',VMrk),
	atom_concat(ArtMrk,'.xml,v',File),
	retractall(art_mrk(_)),
	assert(art_mrk(ArtMrk)).

attr_map(Elem,mrk,VMrk,XMrk) :- var(VMrk),
   member(Elem,[drv,subdrv,snc,subsnc]),
%   atom_codes(AMrk,XMrk),	    
   atomic_list_concat([_ArtMrk|VParts],'.',XMrk),
   atomic_list_concat(VParts,'.',VMrk).

attr_map(Elem,mrk,VMrk,XMrk) :- nonvar(VMrk),
   member(Elem,[drv,subdrv,snc,subsnc]),
   art_mrk(Prefix), % atom_codes(Postfix,VMrk),
%   atom_codes(AMrk,XMrk),
   atomic_list_concat([Prefix,'.',VMrk],XMrk).
   %atom_codes(XMrk,...)???


attr_map(ref,lst,VLst,XLst) :- atom_concat('voko:',VLst,XLst).

attr_map(_,_,Attr,Attr). % :- atom_codes(VAttr,XAttr). % identa transformo de ĉiuj aliaj atributoj


%% elat_map(VrtElem:atom, XmlElem:atom, XmlAttrList:list, XmlAttrVal:list) is nondet
%
% transformoj de elementoj kaj attributoj
% elat_map(VrtElem,XmlElem,XmlAttr)
% En formato Vrt aperanta attributo ne havas nomon,
% tiuj transformreguloj bildigas ĝin al Xml-attributo
%
% transformoj de Vrt-elemento al Xml-elemento inkluzive atributoj
% Oni povas doni kiel lastan argumenton liston de atributoj kun fiksitaj valoroj


% referencoj
elat_map(RefTip,ref,[cel],[tip=RefTip]) :- member(RefTip,[super,sub,malprt,prt,sin,vid,ant,hom]).
elat_map(lst,ref,[lst,cel],[tip=lst]).
%elat_map(lst,ref,[lst,cel,val],[tip=lst]). % kelkaj lst-ref entenas atributon val
elat_map(rdif,ref,[cel],[tip=dif]). % por ne konfuzi dif> kaj ref-tip "dif"
elat_map(rekz,ref,[cel],[tip=ekz]). % por ne konfuzi ekz> kaj ref-tip "ekz"
elat_map(vid,ref,[cel],[]). % tip=vid estas implicita

elat_map(RefTip,refgrp,[],[tip=RefTip]) :- member(RefTip,[super,sub,malprt,prt,sin,vid,ant,hom]).

% uzoj
elat_map(UzoTip,uzo,[],[tip=UzoTip]) :- member(UzoTip,[fak,stl,reg]),!.
elat_map(uklr,uzo,[],[tip=klr]). % uzo klarigo
elat_map(uzo,uzo,[],[]). % uzo sen tipo

% bildoj
elat_map(BldTip,bld,[lok],[tip=BldTip]) :- member(BldTip,[img,svg]).

%aggr_map(trdj,[
%	drv/trd,drv/trdgrp,
%	snc/trp,snc/trdgrp,
%	subdrv/trd,subdrv/trdgrp,
%	subsnc/trd,subsnc/trdgrp]).

aggr_map(trd,[art,subart,drv,subdrv,snc,subsnc],trdj).
aggr_map(trdgrp,[art,subart,drv,subdrv,snc,subsnc],trdj).

% transformo de element-tipoj (blk,lin,inl,var)

eltp_map(vortaro,blk).
eltp_map(art,blk).
eltp_map(subart,blk).
eltp_map(drv,blk).
eltp_map(subdrv,blk).
eltp_map(snc,blk).
eltp_map(subsnc,blk).

eltp_map(dif,blk).
eltp_map(ekz,blk).

eltp_map(kap,lin).

% depends from parent
eltp_map(dif/trdgrp,inl).
eltp_map(dif/trd,inl).
eltp_map(dif/refgrp,inl).
eltp_map(dif/ref,inl).
%eltp_map(dif,_,inl). % ne, ĉar "ekz" estus escepto

eltp_map(trdgrp,lin).
eltp_map(trdgrp/trd,inl).
eltp_map(trd,lin).

% depends from content, if contains element then blk, else inl
eltp_map(fnt,var).

% all other treat as inline
eltp_map(El,inl) :- atom(El).


el_fold(trdgrp/trd).
%el_fold(trd).

el_line_before(drv).

el_trim(trdgrp).
el_trim(trd).
