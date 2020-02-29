:- module(db_revo_download,[
        download/0
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

user:file_search_path(pro, './pro'). % aŭ: current_prolog_flag(home, Home). ...
:- use_module(pro(cfg/agordo)).

download :-
    agordo:get_config(revodb_tmp,TmpFile),
    agordo:get_config(revogh_prefix,Prf),
	github_release(Prf,DownloadUrls),
	member(Url,DownloadUrls), !,
	download_file(Url,TmpFile).

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

github_release(Prf,DownloadUrls):-
    agordo:get_config(revogh_release,Url),
	catch((
		http_open(Url,InStream,[encoding(octet)]),
		json_read_dict(InStream,R),
		findall(
			DlUrl,
			(
				member(A,R.assets),
				%sub_atom(A.name,0,_,_,Prf),
				sub_atom(A.name,_,_,_,Prf),
				DlUrl=A.browser_download_url
			), 
			DownloadUrls
		)),
		_E,
		(format('Malsukcesis trovi la dosierojn en ~w.~n',[Url]), fail)).

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
	)).