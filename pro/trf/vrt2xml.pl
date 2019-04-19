/* -*- Mode: Prolog -*- */
:- consult(parsevrt).
:- ensure_loaded(transform).
:- ensure_loaded(voko_entities).
:- ensure_loaded(xml_quote).


xmlfile('abel-re.xml').
trmfile('abel-re.trm').
vrtfile('abel.vrt').

:- prepare_entity_index.

% test eos
test(1,[]).

% test nl
test(2,[nl(1)]).

% test element
test(3,[start(art),gt,nl(1),text(xyz),nl(2),lt,end(art)]).

% test list element
test(4,[blst,start(art),gt,nl(1),lng(de),col,wht,text('Liste'),nl(2),elst]).

% test multiple attributes element
test(5,[lbr,start(lst),col,attr('zoologiaj_genroj,genr.0o.BIO'),gt,text('Genro'),rbr]).

% test invalid element
test(6,[lbr,start(lst),col,attr('zoologiaj_genroj,genr.0o.BIO'),gt,invalid,text('Genro'),rbr]).

% test some content with invalid item...
test(7,[nl(9),wht,lbr,start(lst),col,attr('zoologiaj_genroj,genr.0o.BIO'),gt,text('Genro'),invalid,
 rbr,wht,text(el),wht,text(la),wht,invalid,text(familio),nl(10)]).

% test list content with multiple translations
test(8,[wht,blst,start(trdj),gt,nl(1), wht(' '), lng(be), col, wht, text("пасека"), com, wht(' '),
	text("пчальнік"), nl(2), wht(' '), elst]).

% test multiple attributes
%test(9,"{lst:zoologiaj_genroj,genr.0o.BIO>Genro}").
test(9,[element(lst, 'zoologiaj_genroj,genr.0o.BIO', ['Genro'])]).

% test traduklisto
test(10,[element(snc,'xxx',[lst(trdj,[],['\n',' ',element(trd,be,[' ',пчала,'\n',' ']),element(trd,bg,[' ',пчела,'\n',' ']),element(trd,br,[' ',gwenanenn,'\n',' '])])])]).

% test trf trd / trim / indent
test(11,[element(snc,'xxx',[lst(trdj,[],['\n','   ',element(trdgrp,fa,['\n',element(trd,[],['  ',زنبور,' ',عسل]),',','\n',element(trd,[],[' ',زنبور,'\n','        ']),'\n'])])])]).

test(12,[element(fak,[],['ZOO'])]).

test(13,[element(lst, 'ĥemiaj_elementoj,elemen.0o.KEM,val=74', [elemento])]).

test(14,`vortaro> iu teksto <!-- jen \n - komento -->\n<vortaro`).

test_tokens(N,T,L) :-
  test(N,Str),
  tokenize:tokenize(Str,T,L).

test_read_vrt(N,Tree,DOM) :-
  test(N,Str),
  read_vrt(Str,Tree,DOM).

test_content(N,Tree,L) :-
  test(N,List),
  phrase(parsevrt:content(Tree),[1|List],[L]).

test_element(N,Tree) :-
  test(N,List),
  parsevrt:parse(List,Tree).

test_transform(N,Tree) :-
  test(N,List),
  writeq(List), write('\n'),
  trf_list(List,Tree),
  writeq(Tree).

test_parse(N,XmlDOM) :-
  test(N,Text),
  parsevrt:read_vrt(Text,_VrtTree,XmlDOM).



test_parse(CnDOM) :-
  vrtfile(VrtFile),
  parsevrt:read_vrt_from_file(VrtFile,VrtTree,XmlDOM),
  trmfile(TrmFile),
  write_term_to_file(TrmFile,VrtTree),
  dom_cnodes(XmlDOM,CnDOM),
  xmlfile(XmlFile),
  write_xmlfile(XmlFile,CnDOM).


write_xmlfile(File,XmlDOM) :-
  open(File,write,Stream,[encoding(utf8)]),
  xml_write(Stream,XmlDOM,[system('../dtd/vokoxml.dtd')]),
  close(Stream).

write_term_to_file(File,ElTree) :-
  open(File,write,Stream),
  writeq(Stream,ElTree),
  close(Stream).

/* *
write_codes_to_file(File,List) :-
  open(File,write,Stream),
  format(Stream,'~s',[List]),
  close(Stream).
* */

