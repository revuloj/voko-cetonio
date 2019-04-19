:- module(xslt_proc,[
    xslt_proc/2,
    xslt_proc/3
]).

test :- process_create(path(ls), ['-l'], []).

/*
Schematically: 

     open(File, write, Out), 
     process_create(Cmd, Args, [stdout(pipe(In))]), 
     thread_create(copy_stream_data(In,Out), _, []), 

Well, you need to do some additional stuff to get everything closed 
nicely. 

This could be automated in library(process). The main downside is that 
that this library is closely compatible with SICStus. Adding this breaks 
this. 

What about adding an async copying predicate so you can use the above, 
replacing the thread_create/3 line with a library call and have it all 
working correctly?  I think it would be something like this: 

copy_stream_data_async(In, Out) :- 
        thread_create(copy_and_close(In, Out), _, 
                      [ detached(true) 
                      ]). 

copy_and_close(In, Out) :- 
        setup_call_cleanup( 
            copy_stream_data(In, Out), 
            call_cleanup(close(In), close(Out))). 

        Cheers --- Jan 

P.s.        Never thought about using call_cleanup(close(In), close(Out)) as 
        a way to reliably close two streams and forward possible exceptions. 
        Should work nicely though. 
        */


test2 :- 
    process_create(path(xsltproc), ['xsl/revohtml.xsl','-'], [stdin(pipe(Xml))]),
    format(Xml,'<?xml version="1.0"?><vortaro></vortaro>',[]),
    close(Xml).


test3 :- 
    open_any(string(Html),write,HtmlStr,_,[]),
    process_create(path(xsltproc), ['xsl/revohtml.xsl','-'], [stdin(pipe(Xml)),stdout(pipe(Out))]),
    copy_stream_data_async(Out,HtmlStr,Html),
    format(Xml,'<?xml version="1.0"?><vortaro></vortaro>',[]),
    close(Xml).

copy_stream_data_async(In, Out,H) :- 
    thread_create(
        copy_and_close(In, Out), _, 
                  [ detached(true),
                    at_exit(write(H))
                  ]). 

copy_and_close(In, Out) :- 
    call_cleanup(
        copy_stream_data(In, Out), 
        call_cleanup(close(In), close(Out))). 



test4 :- 
    process_create(path(xsltproc), ['xsl/revohtml.xsl','-'], [stdin(pipe(Xml)),stdout(pipe(Html)),stderr(pipe(Err))]),
    format(Xml,'<?xml version="1.0"?><vortaro></vortaro>',[]),
    close(Xml),
    read_stream_to_codes(Html,C),
    format('~s',[C]), close(Html),
    read_stream_to_codes(Err,E),
    format('~s',[E]), close(Err).

test5 :- 
    process_create(path(xsltproc), ['xsl/revohtml.xsl','-'], [stdin(pipe(Xml)),stdout(pipe(Html)),stderr(pipe(Err))]),
    format(Xml,'<?xml version="1.0"?><ortaro></vortaro>',[]),
    close(Xml),
    read_stream_to_codes(Html,C),
    format('~s',[C]), close(Html),
    read_stream_to_codes(Err,E),
    format('~s',[E]), close(Err).


xsltproc_status(0,'Neniu eraro, ĉio bone').
xsltproc_status(1,'Mankas argumento por xsltproc').
xsltproc_status(2,'Tro multaj argumentoj por xsltproc').
xsltproc_status(3,'Nekonata argumento por xsltproc').
xsltproc_status(4,'Ne eblis legi aŭ analizi la transformregulojn').
xsltproc_status(5,'Eraro en la transform-reguloj').
xsltproc_status(6,'Eraro en unu el la legitaj dokumentoj').
xsltproc_status(7,'Nesubtenata metodo por xsl:output').
xsltproc_status(8,'Parametro enhavas citilojn kaj simplajn kaj duoblajn').
xsltproc_status(9,'Interna procederaro de xsltproc').
xsltproc_status(10,'Procedo haltigita pro netranssaltebla eraro').
xsltproc_status(11,'Ne eblis skribi la rezulton').
xsltproc_status(N,'Nekonata stato redonita de xsltproc') :- N>11.


test6 :-
    VokoXsl = '/home/revo/voko/xsl/revohtml1.xsl', 
    atom_codes('<?xml version="1.0"?><vortaro>Eĥo</vortaro>',Quoted), 
    xslt_proc(VokoXsl,Quoted,current_output).

xslt_proc(XsltFile,XmlCodes,HtmlCodes) :-
    setup_call_cleanup(

        process_create('/usr/bin/xsltproc', ['--nowrite','--nomkdir',XsltFile,'-'], [
            stdin(pipe(XmlStream)),
            stdout(pipe(HtmlIn)),
            stderr(pipe(ErrIn)),
            process(PID)]),

        (
            set_stream(XmlStream,encoding(utf8)),
            set_stream(HtmlIn,encoding(utf8)),
            set_stream(ErrIn,encoding(utf8)),
            format(XmlStream,'~s',[XmlCodes]),
            close(XmlStream),

            call_cleanup(
                ( 
                    read_stream_to_codes(HtmlIn, HtmlCodes, []),
                    read_stream_to_codes(ErrIn, EC, [])
                ),
                process_wait(PID, Status))
        ),
        (close(HtmlIn), close(ErrIn))
    ),

    debug(xslt(status),'xsltproc status: ~q',[Status]),
    (
        Status = exit(S), S > 0
        -> 
            xsltproc_status(S,Smsg),
            %format(atom(Msg),'Transformstato: ~w~n~s',[Smsg,EC]), 
            %throw(xslt_exception(Msg))
            format(atom(Exc),'Transformstato: ~d - ~w~n',[S,Smsg]),
            format(atom(Msg),'~s',[EC]), 
            throw(xslt_exception(Exc,Msg))
        ;
        true
        %format(HtmlStream,'~s',[HC]) 
    ).



xslt_proc(XsltFile,XmlCodes) :-
    process_create('/usr/bin/xsltproc', ['--nowrite','--nomkdir',XsltFile,'-'], [stdin(pipe(XmlStream))]),
        set_stream(XmlStream,encoding(utf8)),
        format(XmlStream,'~s',[XmlCodes]),
        close(XmlStream).
    
