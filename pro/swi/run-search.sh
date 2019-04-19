#!/bin/bash

# ĉar tiu skripto estas ankaŭ lanĉata de radiko (root)
# kiel servo, necesas iom da manipulado por eltrovi
# la uzanton (ordinare revo aŭ revo-test)

# eltrovu absolutan padon de tiu ĉi skripto
script=$(readlink -f "$BASH_SOURCE")
base=$(dirname "$script")
#base=/home/revo/voko/swi

# eltrovu en kiu, hejmo ni estas
suffix=${base#/home/}
user=${suffix%%/*}

# difinu pliajn variablojn üpr la http-demono

plsrc=${base}/citajho-servo.pl
goal=citajho_servo:daemon
#PL=/usr/bin/env swipl
PL=/usr/bin/swipl
pidfile=/var/lock/swi.serchilo.${user}
#user=revo
home=/home/${user}
etc=${home}/etc
workers=5
if [ "$user" = "revo-test" ]
  then
      port=9000
      syslog=serch-tst
  else
      port=8000
      syslog=serchilo
fi

cd ${base}
${PL} -q -f "${plsrc}" -g "${goal},halt" -t 'halt(1)' -p agordo=${etc} -- \
    --port=${port} --syslog=${syslog} --pidfile=${pidfile} \
    --user=${user} --group=${user}  --workers=${workers} 



