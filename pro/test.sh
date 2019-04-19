#!/bin/bash 
# Absolute path to this script, e.g. /home/user/bin/foo.sh
SCRIPT=$(readlink -f "$BASH_SOURCE")
echo $SCRIPT

# Absolute path this script is in, thus /home/user/bin
SCRIPTPATH=$(dirname "$SCRIPT")
echo $SCRIPTPATH

IFS='/' read -ra PARTS <<< "$SCRIPTPATH"

#echo ${PARTS[2]}

#prefix=${SCRIPTPATH%/voko/swi}
#echo $prefix
#user=${prefix#/home/}
#echo $user

suffix=${SCRIPTPATH#/home/}
echo $suffix
user=${suffix%%/*}

echo $user

if [ "$user" = "revo-test" ]; then port=9090; else port=8080; fi

echo $port


#echo ${${SCRIPTPATH%/voko/swi}#/home/}
