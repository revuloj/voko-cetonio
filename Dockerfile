#### staĝo 1: certigu, ke vi antaŭe kompilis voko-grundo aŭ ŝargis de Github kiel pakaĵo
ARG VERSION=latest
FROM ghcr.io/revuloj/voko-grundo/voko-grundo:${VERSION} as grundo 
  # ni bezonos la enhavon de voko-grundo build poste por kopii jsc, stl, dok


##### staĝo 2: Nun ni kreos la propran procesumon por la redaktilo...
FROM swipl:stable
LABEL Author=<diestel@steloj.de>

# normale: master aŭ v1e ks, 'bin/eldono.sh kreo' metas tion de ekstere per --build-arg
ARG VG_TAG=master
# por etikedoj kun nomo vXXX estas la problemo, ke GH en la ZIP-nomo kaj dosierujo forprenas la "v"
# do se VG_TAG estas "v1e", ZIP_SUFFIX estu "1e", en 'bin/eldono.sh kreo' tio estas jam konsiderata
ARG ZIP_SUFFIX=master

RUN apt-get update && apt-get install -y --no-install-recommends \
    binutils xsltproc sqlite3 unzip curl whois && rm -rf /var/lib/apt/lists/* 

# ne plu bezonata pro oauth2.pl: RUN swipl -g "pack_install(googleclient,[interactive(false)]),halt" -t "halt(1)"

RUN useradd -ms /bin/bash -u 1088 cetonio
WORKDIR /home/cetonio

ADD . ./
# ??? --chown=root:root
#COPY --from=metapost --chown=root:root voko-grundo-master/smb/ /home/cetonio/voko/smb/
#COPY --from=builder --chown=root:root redaktilo-gen.js /home/cetonio/pro/web/
#COPY --from=builder --chown=root:root voko-grundo-master/ /home/cetonio/voko/



# voko-grundo-master/stl/* 
COPY --from=grundo build/ /home/cetonio/voko/

# COPY --from=grundo build/dtd/ /home/cetonio/voko/dtd/
# COPY --from=grundo build/smb/ /home/cetonio/voko/smb/

# eble ni pli bone dividu kiujn CSS-dosierojn ni bezonas en kiu ujo?
# COPY --from=grundo build/stl/ /home/cetonio/voko/stl/
COPY --from=grundo build/stl/ /home/cetonio/web/static/
# COPY --from=grundo build/jsc/ /home/cetonio/voko/jsc/
COPY --from=grundo build/rsj/ /home/cetonio/web/static/

# tio ŝajnas momente superflua, ĉar identa kun voko-grundo-master/xsl (supre)
# sed ĝi povas devii iom se master != ${VERSION}
#COPY --from=grundo build/xsl/ /home/cetonio/voko/xsl/

RUN chown cetonio etc \
#  && curl -LO https://github.com/revuloj/voko-grundo/archive/${VG_TAG}.zip \
#  # && unzip ${VG_TAG}.zip voko-grundo-${ZIP_SUFFIX}/xsl/* voko-grundo-${ZIP_SUFFIX}/dtd/* \ 
#  && unzip ${VG_TAG}.zip voko-grundo-${ZIP_SUFFIX}/cfg/* voko-grundo-${ZIP_SUFFIX}/smb/*.gif voko-grundo-${ZIP_SUFFIX}/owl/voko.rdf \
#  && rm ${VG_TAG}.zip && mv voko-grundo-${ZIP_SUFFIX} voko \
  && cd voko/cfg \
  && curl -LO https://raw.githubusercontent.com/revuloj/revo-fonto/master/cfg/bibliogr.xml \
  && curl -LO https://raw.githubusercontent.com/revuloj/revo-fonto/master/cfg/klasoj.xml \
  && cd /home/cetonio \
  && xsltproc voko/xsl/bibxml.xsl voko/cfg/bibliogr.xml > voko/cfg/biblist.xml && \
    xsltproc voko/xsl/cfg_klasoj.xsl voko/owl/voko.rdf > voko/cfg/klasoj.xml

USER cetonio:users
RUN mkdir -p tmp && mkdir -p sql

# se ni volas uzi la gastigan reton (network_mode: "host) por forsendi retpoŝton
# ni bezonas la eblecon difini la servo-retpordon tie ĉi,
# ĉar 
#   network --driver host 
# ne kunfunkcias kun
#   run --publish gastiga:interna
ENV CETONIO_PORT=8080
ENTRYPOINT ["./bin/docker-entrypoint.sh"]

CMD swipl -s pro/redaktilo-servo.pl -g "redaktilo_servo:daemon" -t halt \
    -p agordo=etc -- --workers=10 --port=${CETONIO_PORT} --no-fork
