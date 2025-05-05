#!/bin/bash

set -x

# Tio ĉi lanĉas la retservon Cetonio loke (sen uzo de docker)
# Por la oficiala aplikaĵo ni intertempe uzas docker-compose 
# (vd. projekton revo-medioj), sed por testi loke / sencimigi 
# tiu ĉi skripto povas esti ankoraŭ utila
# ĉar ni volas interagi kun la kodo ni ne uzas goal "daemon" sed
# "server" - vidu malsupre. Tiam ni povas enŝalti sencimigajn temojn (debug topics)
# kaj voki predikatojn rekte en la interaga komandlinio.
# Por haltigi vi povas uzi ./stop-server.sh

# La sencimigo-temojn vi listas en la komandlinio de SWi-Prolog per:
# list_debug_topics.
# (ial komence necesas dufoje tajpi tion!)
# Per 
# debug(Topic).
# vi enŝaltas sencimigajn informojn pri la koncerna temo, ekz:
# debug(http(_)).
# debug(sqlite).
# debug(db(_)).

# eltrovu absolutan padon de tiu ĉi skripto
script=$(readlink -f "$BASH_SOURCE")
base=$(dirname "$script")/..
#base=/home/revo/voko/swi

# eltrovu en kiu, hejmo ni estas
suffix=${base#/home/}
user=${suffix%%/*}

# difinu pliajn variablojn üpr la http-demono

plsrc=${base}/pro/redaktilo-servo.pl
#PL=/usr/bin/env swipl
PL=/usr/bin/swipl
pidfile=/var/lock/swi.redaktilo.${user}
#user=revo
home=/home/${user}
etc=${home}/etc
workers=2
port=8080
#port=9090

# por sencimigi vi ne volas demonigi la servon
# sed interagi kun ĝia kodo:
goal="redaktilo_servo:server($port)"

export LANGUAGE=eo
export LANG=eo.UTF-8
export VOKO=${home}/voko
export REVO=${home}/revo

# export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

#SAXONJAR=/usr/share/java/Saxon-HE.jar
#SAXONJAR=/usr/share/java/org.apache.servicemix.bundles.saxon-9.4.0.7_1.jar
#SAXON=/usr/share/java
#JINGJAR=/usr/share/java/jing.jar
#JPL=/usr/lib/swi-prolog/lib/jpl.jar

#export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64/server:$JAVA_HOME/jre/lib/amd64:${LD_LIBRARY_PATH}
#export CLASSPATH=$JPL:$SAXONJAR:$JINGJAR:$CLASSPATH
#export PATH=${JAVA_HOME}/bin:${PATH}

cd ${base}
#exec ${PL} -q -f "${plsrc}" -g "${goal},halt" -t 'halt(1)' -p agordo=${etc} -- \
#    --port=${port} --pidfile=${pidfile} \
#    --user=${user} --group=${user} --workers=${workers} 

exec ${PL} -f "${plsrc}" -g "${goal}" -t 'halt(1)' -p agordo=${etc} -- \
    --port=${port} --pidfile=${pidfile} \
    --user=${user} --group=${user} --workers=${workers} 


