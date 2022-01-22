#### staĝo 1: certigu, ke vi antaŭe kompilis voko-grundo aŭ ŝargis de Github kiel pakaĵo
ARG VERSION=2e
FROM ghcr.io/revuloj/voko-grundo/voko-grundo:${VERSION} as grundo 
  # ni bezonos la enhavon de voko-grundo build poste por kopii jsc, stl, dok


##### staĝo 2: Nun ni kreos la propran procesumon por la redaktilo...
FROM swipl:8.4.1
LABEL Author=<diestel@steloj.de>

RUN apt-get update && apt-get install -y --no-install-recommends \
    binutils xsltproc sqlite3 unzip curl && rm -rf /var/lib/apt/lists/* 

# ne plu bezonata pro oauth2.pl: RUN swipl -g "pack_install(googleclient,[interactive(false)]),halt" -t "halt(1)"

RUN useradd -ms /bin/bash -u 1088 cetonio
WORKDIR /home/cetonio

ADD . ./
#  ??? --chown=root:root
#COPY --from=metapost --chown=root:root voko-grundo-master/smb/ /home/cetonio/voko/smb/
#COPY --from=builder --chown=root:root redaktilo-gen.js /home/cetonio/pro/web/
#COPY --from=builder --chown=root:root voko-grundo-master/ /home/cetonio/voko/

RUN chown cetonio etc \
  && curl -LO https://github.com/revuloj/voko-grundo/archive/master.zip \
  && unzip master.zip voko-grundo-master/xsl/* voko-grundo-master/dtd/* \
     voko-grundo-master/cfg/* voko-grundo-master/smb/*.gif voko-grundo-master/owl/voko.rdf \
  && rm master.zip && mv voko-grundo-master voko \
  && cd voko/cfg \
  && curl -LO https://raw.githubusercontent.com/revuloj/revo-fonto/master/cfg/bibliogr.xml \
  && curl -LO https://raw.githubusercontent.com/revuloj/revo-fonto/master/cfg/klasoj.xml

#  voko-grundo-master/stl/* 

COPY --from=grundo build/smb/ /home/cetonio/voko/smb/

# eble ni pli bone dividu kiujn CSS-dosierojn ni bezonas en kiu ujo?
COPY --from=grundo build/stl/ /home/cetonio/voko/stl/
COPY --from=grundo build/stl/ /home/cetonio/pro/web/static/

COPY --from=grundo build/jsc/ /home/cetonio/voko/jsc/
COPY --from=grundo build/rsj/ /home/cetonio/pro/web/static/

COPY --from=grundo build/xsl/ ${HOME_DIR}/files/xsl/
# PLIBONIGU: tion prefere ni jam faru en voko-grundo...?

RUN xsltproc voko/xsl/bibxml.xsl voko/cfg/bibliogr.xml > voko/cfg/biblist.xml && \
    xsltproc voko/xsl/cfg_klasoj.xsl voko/owl/voko.rdf > voko/cfg/klasoj.xml

USER cetonio:users
RUN mkdir -p tmp && mkdir -p sql

# se ni volas uzi la gastigan reton por forsendi retpoŝton
# ni bezonas la eblecon difini la servo-repordon tie ĉi,
# ĉar 
#   network --driver host 
# ne kunfunkcias kun
#   run --publish gastiga:interna
ENV CETONIO_PORT=8080
ENTRYPOINT ["./bin/docker-entrypoint.sh"]

CMD swipl -s pro/redaktilo-servo.pl -g "redaktilo_servo:daemon" -t halt \
    -p agordo=etc -- --workers=10 --port=${CETONIO_PORT} --no-fork
