##### staĝo 1: Ni bezonas TeX kaj metapost por konverti simbolojn al png
FROM silkeh/latex:small as metapost
MAINTAINER <diestel@steloj.de>
COPY mp2png.sh .
RUN apk --update add curl unzip librsvg --no-cache && rm -f /var/cache/apk/* 
RUN curl -LO https://github.com/revuloj/voko-grundo/archive/master.zip \
  && unzip master.zip voko-grundo-master/smb/*.mp
RUN cd voko-grundo-master && ../mp2png.sh # && cd ${HOME}


##### staĝo 2: Ni bezonas Javon kaj Closure-Compiler por kompili la Javo-skript-dosieron
FROM openjdk:jre-slim as builder
MAINTAINER <diestel@steloj.de>

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl && rm -rf /var/lib/apt/lists/* 

ADD . ./

RUN curl -LO https://dl.google.com/closure-compiler/compiler-latest.tar.gz \
  && tar -xvzf compiler-latest.tar.gz 

#RUN java -jar closure-compiler*.jar --dependency_mode LOOSE --js_module_root js_src --js js_src/*.js \
#           --js_output_file redaktilo-gen.js #--entry_point ui_kreo.js 

RUN java -jar closure-compiler*.jar --js_module_root js_src --entry_point ui_kreo \
           --js_output_file redaktilo-gen.js js_src/*.js

RUN curl -LO https://github.com/revuloj/voko-grundo/archive/master.zip \
  && unzip master.zip voko-grundo-master/xsl/* voko-grundo-master/dtd/* voko-grundo-master/cfg/* \
     voko-grundo-master/stl/* voko-grundo-master/owl/voko.rdf && rm master.zip 


##### staĝo 3: Nun ni kreos la propran procesumon por la redaktilo...
FROM swipl:stable
MAINTAINER <diestel@steloj.de>

RUN apt-get update && apt-get install -y --no-install-recommends \
    xsltproc sqlite3 unzip && rm -rf /var/lib/apt/lists/* 

RUN swipl -g "pack_install(googleclient,[interactive(false)]),halt" -t "halt(1)"
# jam enestas en swipl:stable: RUN swipl -g "pack_install(prosqlite,[interactive(false)]),halt" -t "halt(1)"

RUN useradd -ms /bin/bash -u 1088 cetonio
WORKDIR /home/cetonio

ADD . ./
#  ??? --chown=root:root
COPY --from=metapost --chown=root:root voko-grundo-master/smb/ /home/cetonio/voko/smb/
COPY --from=builder --chown=root:root redaktilo-gen.js /home/cetonio/pro/web/
COPY --from=builder --chown=root:root voko-grundo-master/ /home/cetonio/voko/

RUN xsltproc voko/xsl/bibxml.xsl voko/cfg/bibliogr.xml > voko/cfg/biblist.xml && \
    xsltproc voko/xsl/cfg_klasoj.xsl voko/owl/voko.rdf > voko/cfg/klasoj.xml

USER cetonio:users
RUN mkdir -p tmp && mkdir -p sql

CMD ["swipl",\
    "-s","pro/redaktilo-servo.pl",\
    "-g","redaktilo_servo:daemon","-t","halt",\
    "-p","agordo=etc","--",\
    "--workers=10","--port=8080","--no-fork"]
