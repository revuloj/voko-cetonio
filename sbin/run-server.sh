#!/bin/bash

# Tio ĉi lanĉas la retservon Cetonio loke (sen uzo de docker)
# Por la oficiala aplikaĵo ni intertempe uzas docker-compose 
# (vd. projekton revo-medioj), sed por testi loke / sencimigi 
# tiu ĉi skripto povas esti ankoraŭ utila
# Por haltigi vi povas uzi ./stop-server.sh
#
# ĉar tiu skripto estas ankaŭ lanĉata de radiko (root)
# kiel servo, necesas iom da manipulado por eltrovi
# la uzanton (ordinare revo aŭ revo-test)

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
workers=20
if [ "$user" = "revo-test" ]
  then
      port=9090
      syslog=redak-tst
  else
      port=8080
      syslog=redaktilo
fi

# por loka kuro (sen interago kun la kodo)
goal=redaktilo_servo:daemon

export LANGUAGE=eo
export LANG=eo.UTF-8
export VOKO=${home}/voko
export REVO=${home}/revo
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

#SAXONJAR=/usr/share/java/Saxon-HE.jar
SAXONJAR=/usr/share/java/org.apache.servicemix.bundles.saxon-9.4.0.7_1.jar
SAXON=/usr/share/java
JINGJAR=/usr/share/java/jing.jar
JPL=/usr/lib/swi-prolog/lib/jpl.jar

export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64/server:$JAVA_HOME/jre/lib/amd64:${LD_LIBRARY_PATH}
export CLASSPATH=$JPL:$SAXONJAR:$JINGJAR:$CLASSPATH
export PATH=${JAVA_HOME}/bin:${PATH}

cd ${base}
${PL} -q -f "${plsrc}" -g "${goal},halt" -t 'halt(1)' -p agordo=${etc} -- \
    --port=${port} --syslog=${syslog} --pidfile=${pidfile} \
    --user=${user} --group=${user}  --workers=${workers} 



