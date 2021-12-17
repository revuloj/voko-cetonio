#!/bin/bash

sigelilo=$(swipl -g 'agordo:get_config(sigelilo,Sigelilo),write(Sigelilo),halt' gist.pl)

HMAC_red_xml () {
  local red_adr=$1
  local xml=$2
  local hmac=$((echo ${red_adr} && cat ${xml}) | openssl dgst -sha256 -hmac "${sigelilo}");
  echo ${hmac#*= }
}

echo -n "test" > .test && HMAC1s=$(HMAC_red_xml test .test) && rm .test
HMAC1p=$(swipl -q -g 'gist:sigelo(test,test,Sigelo,_),write(Sigelo),halt' gist.pl)
echo "hmac-1: $HMAC1s =? $HMAC1p"

echo -n "test" > .test && HMAC2s=$(HMAC_red_xml 'test@example.com' .test) && rm .test
HMAC2p=$(swipl -q -g "gist:sigelo('test@example.com',test,Sigelo,_),write(Sigelo),halt" gist.pl)
echo "hmac-2: $HMAC2s =? $HMAC2p"

HMAC3s=$(HMAC_red_xml 'test@example.com' art.xml)
HMAC3p=$(swipl -q -g "read_file_to_codes('art.xml',C,[]),atom_codes(X,C),gist:sigelo('wolfram@steloj.de',X,Sigelo,_),write(Sigelo),halt" gist.pl)
echo "hmac-3: $HMAC3s =? $HMAC3p"

LSums=$(swipl -q -g "read_file_to_codes('art.xml',C,[]),atom_codes(X,C),gist:sigelo('wolfram@steloj.de',X,_,LSums),write(LSums),halt" gist.pl)
echo "lsums: $LSums"



