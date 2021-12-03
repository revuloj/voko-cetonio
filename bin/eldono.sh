#!/bin/bash

#
release=2c

# ni komprenas preparo 
target="${1:-helpo}"

PAGE=pro/web/redaktilo.html

case $target in
preparo)
    # kontrolu ĉu la branĉo kongruas kun la agordita versio
    branch=$(git symbolic-ref --short HEAD)
    if [ "${branch}" != "${release}" ]; then
        echo "Ne kongruas la branĉo (${branch}) kun la eldono (${release})"
        echo "Agordu la variablon 'release' en tiu ĉi skripto por prepari novan eldonon."
        exit 1
    fi

    echo "Aktualigante skriptojn al nova eldono ${release}..."
    sed -i 's/Cetonio "[1-9][a-z]";/Cetonio "'${release}'";/' ${PAGE}
    ;;
helpo | *)
    echo "---------------------------------------------------------------------------"
    echo "Per la celo 'preparo' oni povas krei git-branĉon kun nova eldono por tie "
    echo "komenci programadon de novaj funkcioj, ŝanĝoj ktp. Antaŭ adaptu en la kapo de ĉi-skripto"
    echo "la variablojn 'release' kaj 'node_release' al la nova eldono."
    ;;    
esac
