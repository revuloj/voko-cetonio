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
    echo .quit | ${sqlite3} ${sql_dir}/redaktantoj.db -init ${sql_dir}/konto-skemo.sql
    ;;
update-db)
    echo "aktualigante datumbazon de artikoloj el la ĉiutaga eldono..."
    mkdir -p ${tmp_dir}
    /usr/bin/swipl -s ${revodb_upd} -g "${revodb_goal}" -t "halt"
    /usr/bin/unzip ${tmp_dir}/revo-inx.db.tmp.zip -d ${tmp_dir}
    cp ${tmp_dir}/revo-inx.db ${sql_dir}/revo-inx.db
    ;;
redaktantoj)
    # elsutu redaktanto-liston kaj aktualigu la datumbazon per ĝi
    echo "${redaktantoj_trg} <- ${redaktantoj_url}"
    /usr/bin/curl -u ${CGI_USER}:${CGI_PWD} -o ${redaktantoj_trg} ${redaktantoj_url}
    echo "aktualigante sql/redaktatoj.db..."
    /usr/bin/swipl -s ${redaktantoj_upd} -g "${redaktantoj_goal}" -t "halt"
esac
