% xml_diagnosis.pl : XML exception diagnosis.
%
% Copyright (C) 2001-2005 Binding Time Limited
% Copyright (C) 2005-2014 John Fletcher
%
% Current Release: $Revision: 3.9 $
% 
% TERMS AND CONDITIONS:
%
% This program is offered free of charge, as unsupported source code. You may
% use it, copy it, distribute it, modify it or sell it without restriction,
% but entirely at your own risk.

% xml_fault( +Term, ?SubTerm, ?Path, ?Message ) identifies SubTerm as a
% sub-term of Term which cannot be serialized.  Message is an atom naming the
% type of error; Path is a string encoding a list of SubTerm's ancestor
% elements in the form <tag>{(id)}* where <tag> is the element tag and <id> is
% the value of any attribute _named_ id.

xml_fault( Term, Term, [], "Illegal Variable" ) :-
	var( Term ).
xml_fault( xml(Attributes,_Content), Attribute, [], Message ) :-
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Message ).
xml_fault( xml(_Attributes,Content), Culprit, Path, Message ) :-
	xml_content_fault( Content, Culprit, Path, Message ).
xml_fault( Content, Culprit, Path, Message ) :-
	xml_term_fault( Content, Culprit, Path, Message ).
xml_fault( Content, Culprit, Path, Message ) :-
	xml_content_fault( Content, Culprit, Path, Message ).
xml_fault( Term, Term, [], "Illegal Term" ) :-
	\+ Term = xml(_,_).

xml_content_fault( Var, Var, [], "Illegal Variable" ) :-
	var( Var ).
xml_content_fault( [H|_T], Culprit, Path, Message ) :-
	xml_term_fault( H, Culprit, Path, Message ).
xml_content_fault( [_H|T], Culprit, Path, Message ) :-
	xml_content_fault( T, Culprit, Path, Message ).
xml_content_fault( Content, Content, [], "Content must be a list" ) :-
	\+ is_list( Content ).

xml_term_fault( Var, Var, [], "Illegal Variable" ) :-
	var( Var ).
xml_term_fault( pcdata(Chars), Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_term_fault( cdata(Chars), Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_term_fault( namespace(URI,Prefix,Element), Culprit, Path, Message ) :-
	namespace_fault( URI, Prefix, Element, Culprit, Path, Message ).
xml_term_fault( doctype(Name, _Id), Name, [], "DOCTYPE tag must be an atom" ) :-
	\+ atom( Name ).
xml_term_fault( doctype(_Name, Id), Id, [], "DOCTYPE Id is malformed" ) :-
	\+ well_formed_doctype_id( Id ).
xml_term_fault( instructions(Target,_Process), Target, [], "Instructions target must be an atom" ) :-
	\+ atom( Target ).
xml_term_fault( instructions(_Target,Process), Process, [], "Instructions body must be chars" ) :-
	\+ is_chars( Process ).
xml_term_fault( comment(Comment), Comment, [], "Comment must be chars" ) :-
	\+ is_chars( Comment ).
xml_term_fault( Element, Culprit, Path, Message ) :-
	element_fault( Element, Culprit, Path, Message ).
xml_term_fault( Unrecognized, Unrecognized, [], "Unrecognized Term" ) :-
	nonvar( Unrecognized ),
	\+ recognized_type( Unrecognized ).

namespace_fault( URI, _Prefix, _Element, URI, [], "Namespace URI must be an atom" ) :-
	\+ atom( URI).
namespace_fault( _URI, Prefix, _Element, Prefix, [], "Namespace Prefix must be chars" ) :-
	\+ is_chars( Prefix ).
namespace_fault( _URI, _Prefix, Element, Culprit, Path, Message ) :-
	element_fault( Element, Culprit, Path, Message ).

element_fault( element(Tag,_Attributes,_Contents), Tag, [], "Tag must be an atom" ) :-
	\+ atom( Tag ).
element_fault( element(Tag,Attributes,_Contents), Attributes, Path, "Attributes must be instantiated" ) :-
	atom( Tag ),
	fault_path( Tag, [], Path, [] ),
	var( Attributes ).
element_fault( element(Tag,Attributes,_Contents), Attribute, Path, Message ) :-
	atom( Tag ),
	fault_path( Tag, [], Path, [] ),
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Message ).
element_fault( element(Tag,Attributes,Contents), Culprit, Path, Message ) :-
	atom( Tag ),
	fault_path( Tag, Attributes, Path, Path1 ),
	xml_content_fault( Contents, Culprit, Path1, Message ).

attribute_fault( Attribute, "Illegal Variable" ) :-
	var( Attribute ).
attribute_fault( Name=_Value, "Attribute Name must be atom" ) :-
	\+ atom(Name).
attribute_fault( _Name=Value, "Attribute Value must be chars" ) :-
	\+ is_chars( Value ).
attribute_fault( Attribute, "Malformed Attribute" ) :-
	\+ Attribute = (_Name=_Value).

is_chars( Chars ) :-
	is_list( Chars ),
	\+ (member( Char, Chars ), \+ (integer(Char), Char >=0)).

fault_path( Tag, Attributes ) -->
	{atom_codes( Tag, Chars )},
	chars( Chars ),
	fault_id( Attributes ),
	" ".

fault_id( Attributes ) -->
	{member( id=Chars, Attributes ), is_chars( Chars )},
	!,
	"(", chars(Chars), ")".
fault_id( _Attributes ) --> "".

recognized_type( cdata(_Chars) ).
recognized_type( namespace(_URI,_Prefix,_Content) ).
recognized_type( element(_Name,_Attributes,_Content) ).
recognized_type( [] ).
recognized_type( [_H|_T] ).
recognized_type( pcdata(_Chars) ).
recognized_type( comment(_Chars) ).
recognized_type( instructions(_Name,_Chars) ).
recognized_type( doctype(_Tag,_DoctypeId) ).

well_formed_doctype_id( public(Chars1,Chars2) ) :-
	is_chars( Chars1 ),
	is_chars( Chars2 ).
well_formed_doctype_id( public(Chars1,Chars2,DTDLiterals) ) :-
	is_chars( Chars1 ),
	is_chars( Chars2 ),
	well_formed_doctype_literals( DTDLiterals ).
well_formed_doctype_id( system(Chars) ) :-
	is_chars( Chars ).
well_formed_doctype_id( system(Chars,DTDLiterals) ) :-
	is_chars( Chars ),
	well_formed_doctype_literals( DTDLiterals ).
well_formed_doctype_id( local ).
well_formed_doctype_id( local(DTDLiterals) ) :-
	well_formed_doctype_literals( DTDLiterals ).

well_formed_doctype_literals( [] ).
well_formed_doctype_literals( [dtd_literal(Chars)|Literals] ) :-
	is_chars( Chars ),
	well_formed_doctype_literals( Literals ).
