:- consult(vrt2xml).
:- consult(xml2vrt).
:- consult(xmldiff).

%testfile('test.xml').
testfile('abel.xml').

test_cycle :-
  testfile(File),
  load_xml(File,XmlSrc,[space(default)]),
  % transformu DOM al VrtTree
  trf_list(VrtTree,XmlSrc),!,
  % konvertu arbon al teksto
  vrt(VrtTree,VrtList),
  linebreak(VrtList,Text),

  % transformu reen al XmlDOM
  read_vrt(Text,_VrtTree2,XmlDst),

  % komparu
  diff(XmlSrc,XmlDst).




