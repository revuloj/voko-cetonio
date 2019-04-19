/* -*- Mode: Prolog -*- */
:- ensure_loaded(transform).
:- consult(vrt).
:- use_module(spacoj).
:- use_module(parsexml).
:- use_module(loaddtd).

testfile('abelet.xml').
trmout('abelet.trm').
vrtout('abelet.vrt').

test_transform(Output) :-
  load_dtd(VokoDTD),
  testfile(File),
  %load_xml(File,Xml,[space(default)]),
  open(File,read,Stream,[encoding(utf8)]),
  parsexml(Stream,VokoDTD,Xml),
  close(Stream),
  % transformu DOM al VrtTree
  trf_list(VrtTree,Xml),!,
  % sekurigu VrtTree al doserio
  trmout(TrmFile),
  write_term_to_file(TrmFile,VrtTree),
  % konvertu arbon al teksto
  vrt(VrtTree,VrtList),
% PLIBONIGU: linikomencoj ene de dif, ekz ne estas tute glataj
  linebreak(VrtList,Output),
%%%Output=VrtList,
  % sekurigu al dosiero
  vrtout(VrtFile),
  write_codes_to_file(VrtFile,Output).


write_term_to_file(File,ElTree) :-
  open(File,write,Stream,[encoding(utf8)]),
  write(Stream,ElTree),
  close(Stream).

write_codes_to_file(File,List) :-
  open(File,write,Stream,[encoding(utf8)]),
  format(Stream,'~s',[List]),
  close(Stream).


