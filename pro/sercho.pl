
:- module(sercho,
    [ 
        sercho/2,
        bildo_sercho/2
        %bildo_info/1,
        %bildo_info_2/1,
        %bildeto_info/1
    ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_parameters)).

sercho(vikipedio,Sercho) :- !,   
    debug(sercho(what),'>>> VIKIPEDIO: ~w',[Sercho]),
    uri_encoded(query_value,Sercho,SerchoEnc),
    UrlBase = 'https://eo.wikipedia.org/w/api.php?format=json&action=query&generator=search&gsrnamespace=0&gsrlimit=50&prop=extracts&exintro&explaintext&exsentences=1&exlimit=max',
    format(atom(Url),'~w&gsrsearch=~w',[UrlBase,SerchoEnc]),
    % Url= 'http://eo.wikipedia.org/w/api.php?action=query&list=search&format=json&indexpageids=true&prop=info&inprop=url&srsearch=homo&srnamespace=0&srprop=snippet&srlimit=16',
    time(http_open(Url,Stream,[])),
    format('Content-type: application/json~n~n'),
    copy_stream_data(Stream,current_output),
    close(Stream),
    debug(sercho(what),'<<< VIKIPEDIO: ~w',[Sercho]).

sercho(anaso,Sercho) :- !,
    debug(sercho(what),'>>> ANASO: ~w',[Sercho]),
    uri_encoded(query_value,Sercho,SerchoEnc),
    UrlBase = 'https://duckduckgo.com/lite?ia=web&dl=eo',
    format(atom(Url),'~w&q=~w+kaj+la',[UrlBase,SerchoEnc]),
    time(http_open(Url,Stream,[])),
    format('Content-type: text/html~n~n'),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream),
    debug(sercho(what),'<<< ANASO: ~w',[Sercho]).

sercho(Kie,Sercho) :-
    memberchk(Kie,[klasikaj,postaj]),
    agordo:get_url(cikado,Url),
    uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
    atom_concat(Root,'/cikado',Path),
    uri_query_components(Search,[sercho(Sercho),kie(Kie)]),
    uri_components(Url1,uri_components(Scheme,Auth,Path,Search,_)),
    debug(redaktilo(citajho),'url ~q',[Url1]),
    http_open(Url1,Stream,[header(content_type,ContentType)]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).


% see https://commons.wikimedia.org/wiki/Special:ApiSandbox
%     https://commons.wikimedia.org/w/index.php
%     https://commons.wikimedia.org/wiki/Commons:Commons_API
%     https://commons.wikimedia.org/wiki/Commons:Credit_line
%     https://wiki.creativecommons.org/wiki/License_Versions#Detailed_attribution_comparison_chart
%     https://commons.wikimedia.org/wiki/Commons:Attribution_Generator
% /w/api.php?action=query&format=json&list=search&srsearch=korvo&srnamespace=0%7C-2&srlimit=20&srinfo=totalhits%7Csuggestion%7Crewrittenquery&srprop=size%7Cwordcount%7Ctimestamp%7Csnippet

wikimedia_pagho_limo(50).

bildo_sercho(Sercho,JList) :-        
    uri_encoded(query_value,Sercho,SerchoEnc),
    UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
    % kreu serĉo-URL-on 
    wikimedia_pagho_limo(Max),
    % namespaces 0,6,14, see https://commons.wikimedia.org/wiki/Help:Namespaces
    format(atom(Url),'~w&list=search&srnamespace=0%7C6%7C14&srlimit=~d&srprop=snippet&srsearch=~w',[UrlBase,Max,SerchoEnc]),
    time(http_open(Url,Stream,[])),
    json_read(Stream,json(JList)),
    close(Stream),
    debug(wikimedia(search),'Url: ~w~n',[Url]),
    json_write(current_output,json(JList)).
    % kontinuus nur se venus malpli ol Max trovoj, kvankam estas pli...., verŝajne tio ne okazus
    % bildo_sercho_continuation(Url,JList).
   

% continue=json([sroffset=50,continue='-||'])
bildo_sercho_continuation(Url,JList) :-
member(continue=json(Continue),JList) ->
    % estas pli por legi.., faru novan demandon
    wikiapi_continuation_params(Continue,ContParEnc),
    atom_concat(Url,ContParEnc,CUrl),
    time(http_open(CUrl,CStream,[])),
    % copy_stream_data(CStream,current_output)
    json_read(CStream,json(CJList)),
    close(CStream),    
    debug(wikimedia(search),'CUrl: ~w~n',[CUrl]),
    json_write(current_output,json(CJList)),
    bildo_sercho_continuation(Url,CJList) 
; true.

wikiapi_continuation_params([],'').    
wikiapi_continuation_params([Name=Value|Cont],ParamEnc) :-
uri_encoded(query_value,Value,ValEnc),
wikiapi_continuation_params(Cont,ParEnc),
format(atom(ParamEnc),'&~w=~w~w',[Name,ValEnc,ParEnc]).


% /w/api.php?action=query&format=json&prop=imageinfo%7Cpageimages&pageids=513470%7C513472&iiprop=user%7Ccomment%7Cparsedcomment%7Ccanonicaltitle%7Ccommonmetadata%7Cextmetadata&piprop=thumbnail%7Cname%7Coriginal

bildo_info(Paghoj) :-    
    uri_encoded(query_value,Paghoj,PaghojEnc),
    UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
    % unue faru serĉon 
    % namespaces 0,6,14, see https://commons.wikimedia.org/wiki/Help:Namespaces
    format(atom(Url),'~w&prop=imageinfo%7Cpageimages%7Cimages%7Cinfo&inprop=url&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&iiprop=extmetadata&pageids=~w',[UrlBase,PaghojEnc]),
    time(http_open(Url,Stream,[])),
    json_read(Stream,json(JList)),!,
    close(Stream),
    debug(wikimedia(info),'Url: ~w~n',[Url]),
    write("["),
    json_write(current_output,json(JList)),!,
    bildo_info_continuation(Url,JList),
    write("]").

bildo_info_continuation(Url,JList) :-   
member(continue=json(Continue),JList) ->
    % estas pli por legi.., faru novan demandon
    wikiapi_continuation_params(Continue,ContParEnc),
    ContParEnc \= '',
    atom_concat(Url,ContParEnc,CUrl),
    time(http_open(CUrl,CStream,[])),
    % copy_stream_data(CStream,current_output)
    json_read(CStream,json(CJList)),
    close(CStream),
    debug(wikimedia(info),'CUrl: ~w~n',[CUrl]),
    write(","), flush_output,
    json_write(current_output,json(CJList)),
    bildo_info_continuation(Url,CJList)
; true.



bildo_info_2(Dosiero) :-    
    uri_encoded(query_value,Dosiero,DosieroEnc),
    UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
    format(atom(Url),'~w&prop=imageinfo%7Cpageimages%7Cinfo&inprop=url&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&iiprop=extmetadata&titles=~w',[UrlBase,DosieroEnc]),
    time(http_open(Url,Stream,[])),
    json_read(Stream,json(JList)),
    close(Stream),
    debug(wikimedia(info),'Url: ~w~n',[Url]),!,
    %write("["),
    json_write(current_output,json(JList)).
    %bildo_info_continuation(Url,JList),
    %write("]").   

bildeto_info(Dosieroj) :-    
    uri_encoded(query_value,Dosieroj,DosierojEnc),
    UrlBase = 'https://commons.wikimedia.org/w/api.php?action=query&format=json',
    format(atom(Url),'~w&prop=pageimages&piprop=thumbnail%7Cname%7Coriginal&pithumbsize=120&titles=~w',[UrlBase,DosierojEnc]),
    time(http_open(Url,Stream,[])),
    json_read(Stream,json(JList)),
    close(Stream),
    debug(wikimedia(info),'Url: ~w~n',[Url]),!,
    %write("["),
    json_write(current_output,json(JList)).
    %bildo_info_continuation(Url,JList),
    %write("]").       

test_wikimedia_api(Sercho) :-
    bildo_sercho(Sercho,JList),
    member(query=json(Query),JList),
    member(search=SList,Query),
    findall(PId,
        (
        member(json(J),SList),
        member(pageid=PId,J)
        ),
        PIds
    ),
    atomic_list_concat(PIds,'|',PageIds),
    format('pageids: ~q~n',[PageIds]),
    bildo_info_(PageIds).

test_wikimedia_api_1(Paghoj) :-
    bildo_info_(Paghoj).