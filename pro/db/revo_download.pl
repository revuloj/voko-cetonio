:- module(db_revo_download,[
        download/0
    ]).

:- use_module(library(http/http_open)).

user:file_search_path(pro, './pro'). % aŭ: current_prolog_flag(home, Home). ...
:- use_module(pro(cfg/agordo)).


download :-
    agordo:get_config(revodb_zip,UrlPattern),
    agordo:get_config(revodb_tmp,TmpFile),
    get_time(Time),
    once((
	    member(Diff,[0,1,2,3,4,5,6,7]),
	    (
		TimeD is Time - Diff * 24 * 3600,
		format_time(atom(DateStr),'%Y-%m-%d',TimeD),
		format(atom(Url),UrlPattern,[DateStr]),
		download_file(Url,TmpFile)%,
		%! % exit after first success
	    )
	)).


download_file(Url,File) :-
    setup_call_cleanup(
	    (
		catch(
			http_open(Url,InStream,[encoding(octet)]),
			_E,
			(format('~w not found.~n',[Url]), fail)),
		open(File,write,OutStream,[encoding(octet)])
	    ),
	    (
		debug(redaktilo(download),'downloading ~q to ~q',[Url,File]),
                format('downloading ~q to ~q',[Url,File]),
		copy_stream_data(InStream,OutStream)
	    ),
	    (
		close(InStream),
		close(OutStream)
	    )
	).