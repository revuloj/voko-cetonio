#!/bin/bash

# tuj finu se unuopa komando fiaskas 
# - necesas por distingi sukcesan de malsukcesa testaro
set -e

# URL por elŝuti la redaktanto-liston
redaktantoj_url=https://reta-vortaro.de/cgi-bin/admin/redaktantoj.pl
redaktantoj_trg=${HOME}/etc/redaktantoj
# Prolog-skripto por aktualigi redaktantojn
redaktantoj_upd=pro/redaktantoj_upd.pl
redaktantoj_goal=redaktantoj:update_redaktantoj
# por vidi SQL-komandojn:
#redaktantoj_dbg='debug(db(redaktantoj)),redaktantoj:update_redaktantoj.'

# Prolog-skripto por aktualigi serĉdatumbazon
revodb_upd=pro/db/revo_download.pl
revodb_goal=sqlrevo:download

sqlite3=/usr/bin/sqlite3
sql_dir=sql
tmp_dir=tmp
etc_dir=etc

target="${1:-redaktantoj}"

case $target in

kreu-db)
    echo "kreante iniciale la sqlite-datumbazojn en ${sql_dir}..."
    #${sqlite3} ${sql_dir}/revo-inx.db -init ${sql_dir}/revo-skemo.sql
    if [[ -f ${sql_dir}/redaktantoj.db ]]; then
        echo "redaktantoj.db jam ekzistas!"
    else
        echo .quit | ${sqlite3} ${sql_dir}/redaktantoj.db -init bin/konto-skemo.sql
    fi

    if [[ -f ${sql_dir}/submetoj.db ]]; then
        echo "submetoj.db jam ekzistas!"
    else
        echo .quit | ${sqlite3} ${sql_dir}/submetoj.db -init bin/submeto-skemo.sql
    fi
    ;;
update-db)
    echo "aktualigante datumbazon de artikoloj el la ĉiutaga eldono..."
    mkdir -p ${tmp_dir}
    /usr/bin/swipl -s ${revodb_upd} -g "${revodb_goal}" -t "halt"
    /usr/bin/unzip -o ${tmp_dir}/revo-inx.db.tmp.zip -d ${tmp_dir}
    cp ${tmp_dir}/revo-inx.db ${sql_dir}/revo-inx.db
    ls -l ${sql_dir}
    ;;
redaktantoj)
    if [[ -z "$CGI_USER" ]]; then
        echo "Mankas medivariablo CGI_USER, ne eblas elŝuti redaktantojn!"
        exit 1
    fi
    if [[ -z "$CGI_PWD" ]]; then
        echo "Mankas medivariablo CGI_PWD, ne eblas elŝuti redaktantojn!"
        exit 1
    fi
    # elsutu redaktanto-liston kaj aktualigu la datumbazon per ĝi
    echo "${redaktantoj_trg} <- ${redaktantoj_url}"
    /usr/bin/curl -fu ${CGI_USER}:${CGI_PWD} -o ${redaktantoj_trg} ${redaktantoj_url}
    echo "aktualigante sql/redaktatoj.db..."
    /usr/bin/swipl -s ${redaktantoj_upd} -g "${redaktantoj_goal}" -t "halt"
    ;;
subm-pwd)
    if [ "$#" -ne 3 ]; then
        echo "Necesas doni la uzantonomon kaj pasvorton kiel duan kaj trian argumenton!"
        exit 1
    else
        set +x
        echo "$2:$(mkpasswd $3)" > ${etc_dir}/passwd
    fi
    ;; 
#js)
#    # ne plu uzata maniero!!!
#    # closure-compiler el Ubuntu estas tro malnova kaj ne subtenas ESMAScript6!
#    # ni do uzas la kompilion en voko-grundo per npm/JS-kompililo...!
#    echo "kompilante javoskripto-fontojn per closure-compiler..."
#    /usr/bin/closure-compiler \
#        --compilation_level WHITESPACE_ONLY \
#        --language_in ECMASCRIPT6 \
#        --common_js_module_path_prefix js_src \
#        --common_js_entry_module js_src/ui_kreo.js \
#        --js_output_file pro/web/static/redaktilo-2a-gen.js \
#        js_src/*.js
#        # --dependency_mode "LOOSE"
#        # --externs https://code.jquery.com/jquery-3.2.1.min.js
#        # --externs https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css
#        # --externs https://code.jquery.com/ui/1.12.1/themes/south-street/jquery-ui.css
#   ;;
*)
    echo "Celo ne konata! Donu kiel argumenton unu el kreu-db, update-db, redaktantoj, subm-pwd"
esac
