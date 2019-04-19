/* -*- Mode: Prolog -*- */
:- module(xsl_transform,[
	      xsl_transform/3,
	      xsl_exception/2
    ]).

:- use_module(library(jpl)).

% XslUrl, ekz-e 'file:///home/revo/voko/xsl/revohtml2.xsl'

xsl_transform(Xml,XslUrl,OutStr) :-
    atom(Xml),
    jpl_new('java.io.StringReader',[Xml],Is), 
    jpl_new('net.sf.saxon.TransformerFactoryImpl', [], Factory),
    jpl_new('javax.xml.transform.stream.StreamSource', [XslUrl], Xsl),
    jpl_new('javax.xml.transform.stream.StreamSource', [Is], Src),
    jpl_new('java.io.StringWriter',[],Out),
    jpl_new('javax.xml.transform.stream.StreamResult', [Out], Result),
    jpl_call(Factory, newTransformer, [Xsl], Trf),
    jpl_call(Trf, transform, [Src, Result], _),
    jpl_call(Out, toString, [], OutStr).  

xsl_exception(Exc,Text) :-
    jpl_call(Exc, toString, [], Text).

