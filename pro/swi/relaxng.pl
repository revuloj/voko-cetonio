/* -*- Mode: Prolog -*- */
:- module(relaxng,[
	      relaxng/2,
	      relaxng_json/2,
	      rng_exception/2
    ]).

:- use_module(library(jpl)).
:- use_module(library(dcg/basics)).
:- use_module(agordo).

% XslUrl, ekz-e 'file:///home/revo/voko/xsl/revohtml2.xsl'
%  <property name="jing.jar" location="/usr/share/java/jing.jar"/>
%  <property name="jing.class" value="com.thaiopensource.relaxng.util.Driver"/>


% Other tools for validating Relax NG see:
% https://relaxng.org/#validators#validators
% http://ftp.davidashen.net/PreTI/RNV/
% libxml2-utils -> xmllint


/***
test(Out) :-
    read_file_to_codes('/home/revo/revo/xml/art.xml',Codes,[]),
    atom_codes(Xml,Codes),
    relaxng(Xml,'/home/revo/voko/dtd/vokoxml.rnc',Out).

test1(Out,Err,Line,Col) :-
 catch(
	 (
	     read_file_to_codes('./art.xml',Codes,[]),
	     atom_codes(Xml,Codes),
	     relaxng(Xml,'/home/revo/voko/dtd/vokoxml.rnc',Out)
	 ),
	 error(java_exception(Exc),Class), % java_exception( classname, reference_to_exception_object),
	 once(
	     sax_exception(Exc,Class,Line,Col,Err);
	     rng_exception(Exc,Err)
	 )
     ).

test2(Out,MsgList) :-
 catch(
	 (
	     read_file_to_codes('./art2.xml',Codes,[]),
	     atom_codes(Xml,Codes),
	     relaxng(Xml,'/home/revo/voko/dtd/vokoxml.rnc',Out),
	     rng_parse_errors(Out,MsgList)
	 ),
	 error(java_exception(Exc),Class), % java_exception( classname, reference_to_exception_object),
	 once((
	      sax_exception(Exc,Class,Line,Col,Err),
	      MsgList = [Line:Col-Err]
	      ;
	      rng_exception(Exc,Err),
	      MsgList = [0:0-Err]
	 ))
     ).
***/

% rnc('/home/revo/voko/dtd/vokoxml.rnc').
relaxng(Xml,OutStr) :-
    agordo:get_config(relax_rnc,RncFile),
    relaxng(Xml,RncFile,OutStr).

relaxng_json(Xml,Json) :-
    catch(
	(
	    agordo:get_config(relax_rnc,RncFile),
	    relaxng(Xml,RncFile,OutStr),
	    rng_parse_errors(OutStr,MsgList),
	    error_list_json(MsgList,Json)
	 ),
	 error(java_exception(Exc),Class), % java_exception( classname, reference_to_exception_object),
	 once((
	      sax_exception(Exc,Class,Line,Col,Err),
	      error_list_json([Line:Col-Err],Json)
	      ;
	      rng_exception(Exc,Err),
	      error_list_json([0:0-Err],Json)
	 ))
     ).


    
relaxng(Xml,RncFile,OutStr) :-
    atom(Xml),
    % preparu validigilon kun Rnc-skemo
    % rnc(RncFile),
  
    % error handler / out string writer
    jpl_new('java.io.StringWriter',[],Out),
    jpl_new('com.thaiopensource.xml.sax.ErrorHandlerImpl',[Out],Eh),
    jpl_new('com.thaiopensource.util.PropertyMapBuilder',[],Pmb),
    jpl_get('com.thaiopensource.validate.ValidateProperty', 'ERROR_HANDLER', ErrHndl),
    jpl_call(Pmb,put,[ErrHndl,Eh],_),
    jpl_call(Pmb,toPropertyMap,[],Pm),

    % prepare valditor / load schema
    jpl_call('com.thaiopensource.validate.rng.CompactSchemaReader',getInstance,[],Sr),
    jpl_new('com.thaiopensource.validate.ValidationDriver',[Pm,Sr],Vd),
    jpl_call(Vd, fileInputSource, [RncFile], RncIn),
    jpl_call(Vd, loadSchema, [RncIn], @(true)),
    
    % XML fonto    
    jpl_new('java.io.StringReader',[Xml],Is),
    jpl_new('org.xml.sax.InputSource',[Is],XmlIn),
    
    % validigu
    jpl_call(Vd,validate,[XmlIn],_OK),
    jpl_call(Out, toString, [], OutStr).  



rng_exception(Exc,Text) :-
    jpl_call(Exc, toString, [], Text).


sax_exception(Exc,'org.xml.sax.SAXParseException',
	     Line,Col,Msg) :-
    jpl_call(Exc, getLineNumber, [], Line),
    jpl_call(Exc, getColumnNumber, [], Col),
    jpl_call(Exc, getMessage, [], Msg).

error_list_json([Line:Pos-Msg|More],[json([line=Line,pos=Pos,msg=Msg])|MoreJson]) :-
    error_list_json(More,MoreJson).

error_list_json([],[]).    


rng_parse_errors(Lines,MsgList) :-
    atom_codes(Lines,Codes),
    phrase(rng_error_lines(MsgList),Codes),!.

rng_error_lines([Line:0-Msg|More]) --> rng_error_line(Line,Msg),
				     rng_error_lines(More).
rng_error_lines([]) --> [].

rng_error_line(Line,Msg) -->
    "(unknown file):",
    integer(Line),
    ": error: ",
    string_without("\n",M),
    "\n",
    { atom_codes(Msg,M) }.


/*
    <java classpath="${jing.jar}" classname="${jing.class}"
        fork="true"
	output="${jing.errors.txt}">
      <arg value="-c"/>
      <arg file="${voko-relax}"/>
      <arg line="${artikoloj}"/>
			      </java>
*/
      /*

     for (int i = 1; i < args.length; i++) {
-	  if (!driver.validate(ValidationDriver.uriOrFileInputSource(args[i])))
+	  InputSource idoc;
+	  if (args[i].equals("-"))
+	    idoc = new InputSource(System.in);
+	  else
+	    idoc = ValidationDriver.uriOrFileInputSource(args[i]);
+	  if (!driver.validate(idoc))
 	    hadError = true; 


*/

/*
      DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
domFactory.setValidating(true);
DocumentBuilder builder = domFactory.newDocumentBuilder();
builder.setErrorHandler(new ErrorHandler() {
    @Override
    public void error(SAXParseException exception) throws SAXException {
        // do something more useful in each of these handlers
        exception.printStackTrace();
    }
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        exception.printStackTrace();
    }

    @Override
    public void warning(SAXParseException exception) throws SAXException {
        exception.printStackTrace();
    }
});
Document doc = builder.parse("employee.xml");
*/

/*
https://github.com/SWI-Prolog/packages-jpl/issues/3

 1 ?- setenv('CLASSPATH', foo).
 true. 2 ?- 
jpl_call('java.lang.System', getProperty, ['java.class.path'], Path). 
Path = 'c:/swipl-7.3.0/lib/jpl.jar;foo'. In library/jpl.pl Jan prepends the absolute pathname of jpl.jar to CLASSPATH add_jpl_to_classpath :- absolute_file_name(jar('jpl.jar'), [ access(read) ], JplJAR), !, ( getenv('CLASSPATH', Old) -> true ; Old = '.' ), ( current_prolog_flag(windows, true) -> Separator = ';' ; Separator = ':' ), atomic_list_concat([JplJAR, Old], Separator, New), setenv('CLASSPATH', New).


*/
