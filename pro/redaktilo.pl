/* -*- Mode: Prolog -*- */
:- module(redaktilo,[
	      prenu/1,
	      sendu/2,
	      prenu_xml/2,
	      redaktu/1,
	      kontrolu/1
	  ]).
%        preparu/0, 



:- use_module(entities).
%:- use_module(sgml1).
:- use_module(library(http/http_open)).
:- use_module(trf/transform).
%:- use_module(trf/vrt).
:- use_module(spacoj).
:- use_module(trf/parsevrt).
:- use_module(parsexml2).
:- use_module(xml_quote).
:- use_module(loaddtd).
:- consult(xmldiff).

% uzata por pengine_output...
:- use_module(library(pengines)).

:- dynamic vrtbuf/2, xmlsrc/2, vokodtd/2.
:- set_prolog_flag(editor,vim).

/************************
PLIBONIGU:
- en tokenize distingu wht kaj ind('  ') komence de linio

********************/

revo_url('http://retavortaro.de/revo/').


% PLIBONIGU permesu iel doni alian dosierujon, ekz. ~/tmp/revoxml
output_dir('./xml').
%dtd('../dtd/vokosgn.dtd').


%preparu :-
%  load_dtd(VokoDTD),
%  assert(vokodtd(vortaro,VokoDTD)),
%  prepare_entity_index.

%:- preparu.
 

% prenu, redaktu, kontrolu, sendu

prenu(FileName) :-
  revo_url(Revo),
  % antaŭŝargu DTD
%%  vokodtd(vortaro,VokoDTD),
  % elŝutu XML
  atomic_list_concat([Revo,'xml/',FileName,'.xml'],Url),
  http_open(Url,Stream,[]),
  % load_xml(Stream,XmlSrc,[space(default),dtd(VokoDTD)]),
% KOREKTU: necesas normigi la spacojn same kiel faras space(default)...
% eble en trf_list, se en parsexml estas tro malfacila
  %%  parsexml(Stream,VokoDTD,XmlSrc),
  parsexml(Stream,XmlSrc),
  close(Stream),!,
  % sekurigu originalan DOM
  persist_dom(FileName,XmlSrc),
  % konvertu al Vrt
  trf_list(VrtTree,XmlSrc),!,
  vrt(VrtTree,VrtList),
% PLIBONIGU: linikomencoj ene de <dif>, <ekz> ne estas tute glataj
  linebreak(VrtList,Output),
  persist_vrt(FileName,Output).

prenu_xml(FileName,XmlSrc) :-
  revo_url(Revo),
  % antaŭŝargu DTD
%%  vokodtd(vortaro,VokoDTD),
  % elŝutu XML
  atomic_list_concat([Revo,'xml/',FileName,'.xml'],Url),
  http_open(Url,Stream,[]),
  set_stream(Stream,encoding(utf8)),
  read_stream_to_codes(Stream,XmlSrc),
  close(Stream),!.

xml_stream(FileName,XmlStream,Status) :-
  revo_url(Revo),
  % antaŭŝargu DTD
%%  vokodtd(vortaro,VokoDTD),
  % elŝutu XML
  atomic_list_concat([Revo,'xml/',FileName,'.xml'],Url),
  http_open(Url,XmlStream,[status_code(Status)]),!.

unquote_stream(InStream,OutStream) :-
    repeat,
      read_line_to_codes(InStream,Line),
      ( Line == end_of_file
       -> !
       ;
       xml_unquote_cdata(Line,ULine),
       format(OutStream,'~s~n',[ULine]),
       fail
      ).



% montru ...?

redaktu(File) :-
  vrt_tmpfile(File,TmpFile),
  edit(TmpFile).

kontrolu(File) :-
  vrt_tmpfile(File,TmpFile),
  % read back in
  read_file_to_codes(TmpFile,Text,[encoding(utf8)]),
  % transformu reen al XmlDOM
  read_vrt(Text,_VrtTree2,_XmlDst),!.

% montru Vrt el la bufro (tmp file)
vrt(File) :-
   vrt_data(File,Text),
   format('~s',[Text]).

% montru Vrt el la bufro (tmp file)
p_vrt(File) :-
   vrt_data(File,Text),
   %format('~s',[Text]).
   pengine_output(Text).

p_vrt(File,Text) :-
   vrt_data(File,Text).
   %format('~s',[Text]).
   %pengine_output(Text).

% transformu al XML kaj montru
xml(File) :-
  xml_from_vrt(File,Xml),
  xml_write(Xml,[]).

% komparu XML rezultantan el la (redaktita) Vrt
% kun origina DOM strukturo (Xml)
komparu(File) :-
  % komparu
  xml_orig(File,DOMOrig),  
  xml_from_vrt(File,DOMNew),
  diff(DOMOrig,DOMNew).

skribu(File) :-
  xml_from_vrt(File,DOM),
  dom_cnodes(DOM,CNDOM),
  output_dir(Dir),
  atomic_list_concat([Dir,'/',File,'.xml'],OutFile),
  vokodtd(vortaro,VokoDTD),
  open(OutFile,write,Stream,[encoding(utf8)]),
  xml_write(Stream,CNDOM,[dtd(VokoDTD),system('../dtd/vokoxml.dtd')]),
  close(Stream),!.

sendu(_Shangho,_File) :-
  %... repoŝti...
  true. %false.



  
persist_dom(Name,DOM) :-
%  tmp_file_stream(utf8,TmpFile,OutStream),
%  write_canonical(OutStream,dom(DOM)),
%  write(OutStream,'.\n'),
%  close(OutStream),
%  retractall(xmlsrc(Name,_)),
%  assert(xmlsrc(Name,TmpFile)).

    %TODO: testu, ekz. http_in_session(Id)
    % kaj tiam uzu http_session_retractall ktp.
  once((
     check_http_session(true),
     http_session:http_session_retractall(xmlsrc(Name,_)),
     http_session:http_session_assert(xmlsrc(Name,DOM))
     ;
     retractall(xmlsrc(Name,_)),
     assert(xmlsrc(Name,DOM))
   )).

persist_vrt(Name,Vrt) :-
  tmp_file_stream(utf8,TmpFile,OutStream),
  format(OutStream,'~s~n',[Vrt]),
  close(OutStream),
  once((
     check_http_session(true),
     http_session:http_session_retractall(vrtbuf(Name,_)),
     http_session:http_session_assert(vrtbuf(Name,TmpFile))
     ;
     retractall(vrtbuf(Name,_)),
     assert(vrtbuf(Name,TmpFile))
   )).

vrt_data(Name,TextData) :-
   vrt_tmpfile(Name,TmpFile),
   read_file_to_codes(TmpFile,TextData,[encoding(utf8)]).

xml_from_vrt(Name,DOM) :-
  vrt_data(Name,Text),
  read_vrt(Text,_VrtTree2,DOM),!.

xml_orig(Name,DOM) :-
   xmlsrc(Name,DOM).
%   dom_tmp_file(Name,TmpFile),
%   open(TmpFile,read,Stream,[]),
%   read(Stream,dom(DOM)),
%   close(Stream).

vrt_tmpfile(Name,TmpFile) :-
  vrtbuf(Name,TmpFile).

%get_dom(Name,DOM) :-
%  xmlsrc(Name,DOM).
  
%  format('~s~n',[Output]).

check_http_session(Bool) :-
  current_predicate(http_session:http_in_session/1) -> Bool = true; Bool = false.

test_unquote(1) :-
    prenu_xml(abel,XmlSrc),
    time(xml_unquote_cdata(XmlSrc,X1)), !,
    atom_codes(X2,X1),
    writeln(X2).


test_unquote(2) :-
    xml_stream(abel,XmlStream,_Status),
    repeat,
      read_line_to_codes(XmlStream,Line),
      ( Line == end_of_file
       -> !
       ;
       xml_unquote_cdata(Line,ULine),
       format('~s~n',[ULine]),
       fail
      ),
    close(XmlStream).


test_entity('gcirc','ĝ').

test_unquote_quote(1) :-
    prenu_xml(abel,XmlSrc),
    time(xml_unquote_cdata(XmlSrc,X1)), !,
    %atom_codes(X2,X1),
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    time(xml_quote_cdata(X1,Y1,ReverseEntInx,EntVal1Inx,utf8)), !,
    format('~s~n',[Y1]).

test_unquote_quote(2) :-
    xml_quote:reverse_entity_index(redaktilo:test_entity,ReverseEntInx),
    xml_quote:entity_value_1_index(redaktilo:test_entity,EntVal1Inx),
    atom_codes('aĝo',C),
    xml_quote_cdata(C,C1,ReverseEntInx,EntVal1Inx,utf8),
    format('~s~n',[C1]).

test_unquote_quote(3) :-
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    atom_codes('<trd lng="ja">ミツバチ [<ind>みつばち</ind>]</trd> <trd lng="be">вальфрам</trd>',Codes),
    time(xml_quote_cdata(Codes,C1,ReverseEntInx,EntVal1Inx,utf8)),
    format('~s~n',[C1]).

test_unquote_quote(4) :-
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    xml_stream(abel,XmlStream,_Status),
    set_stream(XmlStream,encoding(utf8)),
    repeat,
      read_line_to_codes(XmlStream,Line),
      ( Line == end_of_file
       -> !
       ;
       xml_unquote_cdata(Line,ULine),
       xml_quote_cdata(ULine,Line1,ReverseEntInx,EntVal1Inx,utf8),
       % format('~s~n',[Line1]),
       (Line = Line1
	-> true
	;
	format('1: ~s~n',[Line]),
	format('2: ~s~n',[Line1]),
	format('#! Line != Line1!~n~n')
       ),
       fail
      ),
    close(XmlStream).
  

test_unquote_quote(5) :-
    prenu_xml(nok,XmlSrc),
    time(xml_unquote_cdata(XmlSrc,X1)), !,
    %atom_codes(X2,X1),
    get_entity_index(ReverseEntInx,_EntValLenInx,EntVal1Inx),
    time(xml_quote_cdata(X1,Y1,ReverseEntInx,EntVal1Inx,utf8)), !,
    format('~s~n',[Y1]).
