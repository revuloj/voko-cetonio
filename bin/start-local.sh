#!/bin/bash

echo "AVERTO: lanĉante la redaktilo-servon loke kun limigita funkcio: nur por testado dum programado!!!"
echo "REDAKTANTO_RETPOSHTO: $REDAKTANTO_RETPOSHTO"
echo ""
echo "Vi povas peti sencimigajn informojn per debug(redaktilo(_), debug(http(_). kaj poste lanĉi la servon mem"
echo "per server(8080)."
swipl -p agordo=cfg -p agordo=etc pro/redaktilo-servo.pl
