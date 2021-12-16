#!/bin/bash

# URL por elŝuti la redaktanto-liston
redaktantoj_url=https://reta-vortaro.de/cgi-bin/admin/redaktantoj.pl
redaktantoj_trg=${HOME}/etc/redaktantoj
# Prolog-skripto por aktualigi redaktantojn
redaktantoj_upd=pro/redaktantoj_upd.pl
redaktantoj_goal=redaktantoj:update_redaktantoj
# Prolog-skripto por aktualigi serĉdatumbazon
revodb_upd=pro/db/revo_download.pl
revodb_goal=sqlrevo:download

sqlite3=/usr/bin/sqlite3
sql_dir=sql
tmp_dir=tmp

target="${1:-redaktantoj}"

case $target in

kreu-db)
    echo "kreante iniciale la sqlite-datumbazojn..."
    #${sqlite3} ${sql_dir}/revo-inx.db -init ${sql_dir}/revo-skemo.sql
    echo .quit | ${sqlite3} ${sql_dir}/redaktantoj.db -init bin/konto-skemo.sql
    ;;
update-db)
    echo "aktualigante datumbazon de artikoloj el la ĉiutaga eldono..."
    mkdir -p ${tmp_dir}
    /usr/bin/swipl -s ${revodb_upd} -g "${revodb_goal}" -t "halt"
    /usr/bin/unzip -o ${tmp_dir}/revo-inx.db.tmp.zip -d ${tmp_dir}
    cp ${tmp_dir}/revo-inx.db ${sql_dir}/revo-inx.db
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
    /usr/bin/curl -u ${CGI_USER}:${CGI_PWD} -o ${redaktantoj_trg} ${redaktantoj_url}
    echo "aktualigante sql/redaktatoj.db..."
    /usr/bin/swipl -s ${redaktantoj_upd} -g "${redaktantoj_goal}" -t "halt"
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
esac
