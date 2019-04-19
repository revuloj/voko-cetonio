/* -*- Mode: Prolog -*- */

xml_write(Stream,DOM,Options) :-
  
element(Tag,Attr,Content) --> "<", tag(Tag), " ", attr(Attr), ">", content(Content), "</", tag(Tag), ">".

tag(Tag) --> {atom_codes(Tag,T)}, T.


  
