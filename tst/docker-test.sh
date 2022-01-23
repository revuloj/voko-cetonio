#!/bin/bash

# tuj finu se unuopa komando fiaskas 
# - necesas por distingi sukcesan de malsukcesa testaro
set -e

docker_image="${1:-voko-cetonio:latest}"

# plej multajn funkciojn ni ne povas testi izolite, ĉar bezoniĝas aldonaj servoj
# voko-cikado por serĉado, voko-akrido kaj voko-grilo por kontrolado, sed
# ni povas lamenaŭ fari fumteston ĉu la servilo donas al ni la lanĉan paĝon

# preparu arbitran sekreton
echo "ajax_secret('arbiTrA007')." > .cetonio-sekreto

# lanĉi la test-procezujon
docker run -p 8080 \
  -e REDAKTILO_URL=http://ludoviko.zam/redaktilo \
  -e REDAKTANTO_RETPOSHTO=lazaro@ludoviko.zam \
  -v $(pwd)/.cetonio-sekreto:/run/secrets/voko-cetonio.sekret-agordo \
  --name cetonio-test --rm -d ${docker_image}

# atendi, ĝis ĝi ricevis retpordon
while ! docker port cetonio-test
do
  echo "$(date) - atendante retpordon"
  sleep 1
done

DPORT=$(docker port cetonio-test | head -n 1)
HPORT=${DPORT/#*-> }

echo "retpordo:" $HPORT
echo "Lanĉo de la servo eble daŭras iomete..."

# https://superuser.com/questions/272265/getting-curl-to-output-http-status-code
while ! curl -I "http://$HPORT/" 2> /dev/null
do
  echo "$(date) - atendante malfermon de TTT-servo"
  sleep 3
done

# momente ni nur testas, ĉu la retpetoj estas sukcesaj. Uzante 'jq' ks
# ni povus ankaŭ pli detale rigardi ĉu la enhavo estas kiel atendita...

echo ""; echo "Petante indeks-paĝon..."
curl -fsI "http://$HPORT/"

echo ""; echo "Petante Revo-piktogramon..."
curl -fsI "http://$HPORT/redaktilo/static/revo.png"

echo ""; echo "Petante piktogramon por fako..."
curl -fsI "http://$HPORT/redaktilo/smb/ESP.png"

echo ""; echo "Petante klasliston..."
curl -fsI "http://$HPORT/redaktilo/voko/klasoj.xml"

echo ""; echo "Petante lingvoliston..."
curl -fsI "http://$HPORT/redaktilo/voko/lingvoj.xml"

echo ""; echo "Petante bibliografion..."
curl -fsI "http://$HPORT/redaktilo/voko/biblist.xml"

echo ""; echo "Petante indeks-paĝon kaj akiru la seanco-kuketon..."
curl -L -c .cetonio-kuketo -fsI "http://$HPORT/"

echo ""; echo "Petante preferatajn lingvojn..."
curl -b .cetonio-kuketo -fs "http://$HPORT/redaktilo/red/revo_preflng" -H 'Accept-Language: eo,fr,de-DE;q=0.8,en-US;q=0.5,en;q=0.3'

echo ""; echo "Forigi..."
docker kill cetonio-test
rm .cetonio-sekreto
rm .cetonio-kuketo