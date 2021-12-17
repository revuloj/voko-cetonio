:- use_module('redaktilo-servo').
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).

test:-
    agordo:get_url(grilo,Url),
    Xml = '<?xml version="1.0"?><vortaro><art>\n<drv><dif>Test</dif></drv>\n</art>\n</vortaro>',
    format('url: ~w, xml:~w~n',[Url,Xml]),
    http_open(Url,Stream,[header(content_type,ContentType),post(atom(Xml))]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).

test:-
    agordo:get_url(grilo,Url),
    Xml = '<?xml version="1.0"?><vortaro>\n<art mrk="x">\n<kap>test</kap>\n<drv mrk="x.0o"><kap><tld/>o</kap><dif>Test</dif></drv>\n</art>\n</vortaro>\n',
    format('url: ~w, xml:~w~n',[Url,Xml]),
    http_open(Url,Stream,[header(content_type,ContentType),post(atom(Xml))]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    copy_stream_data(Stream,current_output),
    close(Stream).

test :-
    %http_read_json(Request, JSON, [json_object(dict)]),
    atom_json_dict('{"1": "praĉevalidoj", "5": "henas iihiii"}',JSON,[]),
    agordo:get_url(akrido,Url),
    uri_components(Url,uri_components(Scheme,Auth,Root,_,_)),
    atom_concat(Root,'/analinioj',Path),
    uri_components(Url1,uri_components(Scheme,Auth,Path,'',_)),
    format('url ~q',[Url1]),
    http_post(Url1, json(JSON), Reply, []),
    format('~q~n',[Reply]).


test_k:-
    agordo:get_url(grilo,Url),
    Xml = '<?xml version="1.0"?><vortaro>\nart mrk="x">\n<kap>test</kap>\n<rv mrk="x.0o"><kap><tld/>o</kap><dif>Test</dif></drv>\n</art>\n</vortaro>\n',
    format('url: ~w, xml:~w~n',[Url,Xml]),
    http_open(Url,Stream,[header(content_type,ContentType),post(atom(Xml))]),
    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    set_stream(current_output,encoding(utf8)),
    write(current_output,'['),
    %copy_stream_data(Stream,current_output),
    redaktilo_servo:err_text_json(Stream,current_output),
    write(current_output,']'),
    close(Stream).