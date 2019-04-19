/* -*- Mode: Prolog -*- */
:- use_module(library(jpl)).

    /*

https://trac.lal.in2p3.fr/Quattor/browser/QWG/External/saxon6-5-5/samples/java/TraxExamples.java

      TransformerFactory tfactory = TransformerFactory.newInstance();
354	
355	        // Create a transformer for the stylesheet.
356	        Transformer transformer =
357	            tfactory.newTransformer(new StreamSource(xslID));
358	
359	        // Transform the source XML to System.out.
360	        transformer.transform(new StreamSource(sourceID),
361	                              new StreamResult(System.out));
362	    }

    */

/*
 <xslt in="${v.bazo.xml}/${dosiero}.xml" out="${v.tmp}/art/${dosiero}.xml"
       force="true" style="${v.voko.xsl}/revo_tez.xsl" classpathref="saxon.classpath">
       <factory name="net.sf.saxon.TransformerFactoryImpl"/>
    </xslt>

    <xslt in="${v.tmp}/art/${dosiero}.xml" out="${v.tmp}/art/${dosiero}.html" 
       style="${v.voko.xsl}/revohtml2.xsl" classpathref="saxon.classpath">
         <param name="xml-ref-pado" expression="${v.bazo.xml}"/>
       <factory name="net.sf.saxon.TransformerFactoryImpl"/>
    </xslt>
*/


test :-
    jpl_new('net.sf.saxon.TransformerFactoryImpl', [], Factory),
    jpl_new('javax.xml.transform.stream.StreamSource', ['file:///home/revo/voko/xsl/revohtml2.xsl'], Xsl),
    jpl_new('javax.xml.transform.stream.StreamSource', ['file:///home/revo/revo/xml/abel.xml'], Src),
    jpl_get('java.lang.System', out, Out),
    jpl_new('javax.xml.transform.stream.StreamResult', [Out], Result),
    jpl_call(Factory, newTransformer, [Xsl], Trf),
    jpl_call(Trf, transform, [Src, Result], _).

% jpl_new(array(byte), [1,1,2,3,5,8], A), jpl_array_to_list(A, Ds).

test2 :-
    open('/home/revo/revo/xml/abel.xml',read,Stream,[encoding(ascii)]),
    read_stream_to_codes(Stream,Codes),
    close(Stream),

    %atom_codes('<xml test>',Codes),
    %jpl_new(array(byte),Codes,Array),
    %jpl_new('java.io.ByteArrayInputStream',[Array],Is),

    atom_codes(Atom,Codes),
%    jpl_new('java.lang.String',[Atom],InStr),
    jpl_new('java.io.StringBufferInputStream',[Atom],Is), % use StringReader instead...
    
    jpl_new('net.sf.saxon.TransformerFactoryImpl', [], Factory),
    jpl_new('javax.xml.transform.stream.StreamSource', ['file:///home/revo/voko/xsl/revohtml2.xsl'], Xsl),
    jpl_new('javax.xml.transform.stream.StreamSource', [Is], Src),
    jpl_get('java.lang.System', out, Out),
    jpl_new('javax.xml.transform.stream.StreamResult', [Out], Result),
    jpl_call(Factory, newTransformer, [Xsl], Trf),
    jpl_call(Trf, transform, [Src, Result], _). 


xml(A) :-
    atomic_list_concat(['<?xml version="1.0"?>',
		       '<!DOCTYPE vortaro SYSTEM "../dtd/vokoxml.dtd">',
		       '<vortaro><art mrk="$Id: abel.xml,v 1.67 2015/07/04 08:37:29 revo Exp $">',
		       '<kap>  <ofc>*</ofc><rad>abel</rad>/o</kap>',
		       '<drv mrk="abel.0o">  <kap><ofc>*</ofc><tld/>o</kap><fnt>F</fnt>',
		       '<uzo tip="fak">ZOO</uzo>  <snc>',
		       '<dif> Eĥoŝanĝo ĉiuĵaŭde:     <ref tip="lst" cel="genr.0o.BIO"      lst="voko:zoologiaj_genroj">Genro</ref>',
		       'el la familio    </dif>  </snc></drv></art></vortaro>'],A).


test3(OutStr) :-
    xml(Atom),
    jpl_new('java.io.StringReader',[Atom],Is), 
    
    jpl_new('net.sf.saxon.TransformerFactoryImpl', [], Factory),
    jpl_new('javax.xml.transform.stream.StreamSource', ['file:///home/revo/voko/xsl/revohtml2.xsl'], Xsl),
    jpl_new('javax.xml.transform.stream.StreamSource', [Is], Src),

    jpl_new('java.io.StringWriter',[],Out),
    jpl_new('javax.xml.transform.stream.StreamResult', [Out], Result),

    jpl_call(Factory, newTransformer, [Xsl], Trf),
    jpl_call(Trf, transform, [Src, Result], _),

    jpl_call(Out, toString, [], OutStr). 
