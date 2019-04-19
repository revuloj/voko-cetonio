:- use_module(library(dcg/basics)).

test(0,"").

test(1,
  "art::mirh> mirho dif::{Rezino} <art").

test(2,
  "art::mirh> ofc{*}mirh/o
  drv> ~o
    snc>Rezino bonodora<snc
 <drv
 <art").
 

test_lex(N,List) :-
  test(N,Str),
  format('~s~n',[Str]),
  phrase(tokens(List),Str).
 
test_parse(N,DOM) :-
  test(N,Str),
  format('~s~n',[Str]),
  phrase(tokens(List),Str),!,
  phrase(block(Tree),List),!,
  dump_tree(Tree),
  trf_list([Tree],DOM),
/**
to be done before:

change Attr like "xxx" to attr='xxx'
change text code lists to real strings or atoms `xxx` -> "xxx" or 'xxx'

***/

  xml_is_dom(DOM),
%    new_memory_file(Handle),
%  open_memory_file(Handle, write, Stream),
  open('voko_.xml',write,Stream),
  xml_write(Stream, DOM, []),
  close(Stream).
%  open_memory_file(Handle, read, Rdr, [free_on_close(true)]),
  % load_xml_file(stream(Rdr), [Xml]),  
%  close(Rdr),
%  print(Xml).
  

test_reverse(N,ReverseList) :-
  test(N,Str),
  format('~s~n',[Str]),
  phrase(tokens(List),Str),!,
  phrase(block(Tree),List),!,
  dump_tree(Tree),
  trf_list([Tree],DOM),
  trf_list([Reverse],DOM),
  dump_tree(Reverse),
  phrase(block(Reverse),ReverseList),!,
% not working  ...
%  phrase(tokens(ReverseList),RevStr),
%  format('~s~n',[RevStr]).
  output(ReverseList). 
  
  
dump_tree([]).  
dump_tree(element(El,Attr,Content)) :-
   format('~w::~s>~n',[El,Attr]),
   dump_tree_list(Content),
   format('<~w~n',[El]).
dump_tree(Text) :-
  atomic(Text),
  format('~w',[Text]).
%dump_tree(Text) :-
%  is_list(Text),
%  format('~s',[Text]).
  
dump_tree_list([]) :- format('~n').  
dump_tree_list([First|Rest]) :-
   dump_tree(First),
   dump_tree_list(Rest).
   
% el-attr mappings
elat_map(art,art,mrk).   
elat_map(drv,drv,mrk).   
elat_map(snc,snc,mrk).   
   
%elat_map(El,TEl,Attr,[]) :- elat_map(El,TEl,Attr).   
elat_map(super,ref,cel,[tip=super]).   
elat_map(sub,ref,cel,[tip=sub]). 
  
   
%%%% transform to DOM
trf_item(element(Elem,Attr,Content),element(TElem,TAttr,TContent)) :-
  trf_elat(Elem,Attr,TElem,TAttr),
  %trf_attr(Elem,Attr,TAttr),
  trf_list(Content,TContent).
  
trf_item(Text,Text) :- atomic(Text).

trf_elat(Elem,[],TElem,[]) :-
  elat_map(Elem,TElem,_),!.  

trf_elat(Elem,[],TElem,AttrList) :-
  elat_map(Elem,TElem,_,AttrList),!.  

trf_elat(Elem,AttrVal,TElem,[TAttr=AttrVal]) :-
  elat_map(Elem,TElem,TAttr),!.  

trf_elat(Elem,AttrVal,TElem,[TAttr=AttrVal|AttrList]) :-
  elat_map(Elem,TElem,TAttr,AttrList),!.  

trf_elat(Elem,[],Elem,[]).
  
  
  /**
trf_attr(_,[],[]).

trf_attr(art,Attr,[mrk=Attr]) :- atomic(Attr).
trf_attr(drv,Attr,[mrk=Attr]) :- atomic(Attr).
trf_attr(snc,Attr,[mrk=Attr]) :- atomic(Attr).
**/



trf_list([],[]).
trf_list([First|Rest],[TFirst|TRest]) :-
   trf_item(First,TFirst),
   trf_list(Rest,TRest).
   
   
   

lookahead(T), T --> T.
%lookahead(T1), [T] --> T.

%%%%%%%%%%
% lexer (tokenizer) rules

lh_gt --> lookahead(">"). 
lh_dcol --> lookahead("::").
lh_lbr --> lookahead("{").

gt --> ">".
lt --> "<".
dcol --> "::".
lbr --> "{".
rbr --> "}".

tag(art) --> "art".
tag(drv) --> "drv".
tag(snc) --> "snc".
tag(dif) --> "dif".
tag(ofc) --> "ofc".

token([start(Tag),gt]) --> tag(Tag), gt.
token([start(Tag)]) --> tag(Tag), lh_dcol.
token([start(Tag)]) --> tag(Tag), lh_lbr.
token([lt,end(Tag)]) --> lt, tag(Tag).

token([dcol,attr(Attr),gt]) --> dcol, string_without(">",A), gt, { string_codes(Attr,A) }.
token([dcol,attr(Attr)]) --> dcol, string_without("{",A), lh_lbr, { string_codes(Attr,A) }.

token([lbr,text(Text),rbr]) --> lbr, string_without(" }<",T), rbr, { string_codes(Text,T) }.
token([nl]) --> whites, "\n".
token([wht]) --> white, whites.
token([tld]) --> "~".
token([text(Text)]) --> string_without(" \n\r\t\\~{}<",T), { T \= [], string_codes(Text,T) }.

% fall back to error if no other matches
token([]) --> call(lex_error_).

lex_error_(Rest,Rest) :- length(Rest,L), throw(error(syntax_error_at,Rest,L)).


tokens([]) --> eos.
tokens(Tokens) --> 
	token(T), 
	{ T\=[], format('~k~n',[T]) }, 
	tokens(Rest), 
	{ append(T,Rest,Tokens) }.
	
%%%%%%%%%%%%%%%
% output tokens

output([]).
output([Token|Rest]) :-
  output(Token),
  output(Rest).
  
output(start(Tag)) :- write(Tag).
output(dcol) :- write('::').
output(attr(Attr)) :- write(Attr).
output(gt) :- write('>').
output(lt) :- write('<').
output(end(Tag)) :- write(Tag).
output(lbr) :- write('{').
output(rbr) :- write('}').
output(text(Text)) :- write(Text).
output(tld) :- write('~').
output(wht) :- write(' ').
output(nl) :- write('\n').

% fallback to error
output(T) :- throw(unexpected_token(T)).


%%%%%%%%%%
% parser rules

%  `art::mirh> ofc{*}mirh/o\c\n
%  drv> ~o
%    snc>Rezino bonodora<snc
% <drv
% <art`).

block(element(Tag,[],Content)) --> [start(Tag),gt], content(Content), [lt, end(Tag)].
block(element(Tag,Attr,Content)) --> [start(Tag), dcol, attr(Attr), gt], content(Content), [lt, end(Tag)].

% TODO: for inverse direction, i,e, generating instead parsing, 
% need some logic when to ouput tag{} and when tag>...<tag
% {} only containing simple content..., no other blocks/tags, now newlines(?)
block(element(Tag,[],Content)) --> [start(Tag), lbr], content(Content), [rbr].
block(element(Tag,Attr,Content)) --> [start(Tag), dcol, attr(Attr), lbr], content(Content), [rbr].

content([]) --> []. % eos?
content([" "|Rest]) --> [wht], content(Rest).
content(["\n"|Rest]) --> [nl], content(Rest).
content(["~"|Rest]) --> [tld], content(Rest).
content([Block|Rest]) --> block(Block), content(Rest).
content([Text|Rest]) --> [text(Text)], content(Rest).

% fall back to error if no other matches
% does not work this way as end of block would cause error
% errors nees to be pushed back up to caller instead somehow as a kind of stacktrace...
%content([]) --> call(parse_error_).
%parse_error_(Rest,Rest) :-
%  Rest = [First|_],
%  throw(error(invalid_content_at(First))).
