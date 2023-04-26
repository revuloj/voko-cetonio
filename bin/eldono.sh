#!/bin/bash

# Tio estas la eldono de voko-grundo kontraŭ kiu ni kompilas ĉion
# ĝi devas ekzisti jam kiel git-tag (kaj sekve kiel kodarĥivo kaj procezujo) en Github
# en celo "preparo" ni metas tiun eldonon ankaŭ por dosiernomoj kc. de voko-araneo
# Ni ankaŭ supozas, ke nova eldono okazas en git-branĉo kun la sama nomo
# Ĉe publikigo marku la kodstaton per etikedo (git-tag) v${eldono}.
# Dum la realigo vi povas ŝovi la etikedon ĉiam per celo "etikedo".
eldono=2g

# ni komprenas preparo 
target="${1:-helpo}"

PAGE=pro/web/redaktilo.html

case $target in
preparo)
    # kontrolu ĉu la branĉo kongruas kun la agordita versio
    branch=$(git symbolic-ref --short HEAD)
    if [ "${branch}" != "${eldono}" ]; then
        echo "Ne kongruas la branĉo (${branch}) kun la eldono (${eldono})"
        echo "Agordu la variablon 'eldono' en tiu ĉi skripto por prepari novan eldonon."
        exit 1
    fi

    echo "Aktualigante skriptojn al nova eldono ${eldono}..."
    sed -i 's,/redaktilo-[1-9][a-z]-min\.,/redaktilo-'${eldono}'-min\.,g' ${PAGE}
    sed -i 's/Cetonio [1-9][a-z]/Cetonio '${eldono}'/' ${PAGE}
    #sed -i 's/ARG VERSION=[1-9][a-z]/ARG VERSION='${eldono}'/' Dockerfile
    ;;
kreo)
    echo "Kreante lokan procezujon (por docker) voko-cetonio"
    docker pull ghcr.io/revuloj/voko-grundo/voko-grundo:${eldono}
    docker build --build-arg VERSION=${eldono} --build-arg VG_TAG=v${eldono} --build-arg ZIP_SUFFIX=${eldono} \
        -t voko-cetonio .
    ;;
etikedo)
    echo "Provizante la aktualan staton per etikedo (git tag) v${eldono}"
    echo "kaj puŝante tiun staton al la centra deponejo"
    git tag -f v${eldono} && git push && git push --tags -f
    ;;    
helpo | *)
    echo "---------------------------------------------------------------------------"
    echo "Per la celo 'preparo' oni povas krei git-branĉon kun nova eldono por tie "
    echo "komenci programadon de novaj funkcioj, ŝanĝoj ktp. Antaŭ adaptu en la kapo de ĉi-skripto"
    echo "la variablojn 'eldono' kaj 'node_eldono' al la nova eldono."
    ;;    
esac
