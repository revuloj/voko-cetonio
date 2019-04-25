# Ni bezonas Javon kaj Closure-Compiler por kompili la Javo-skript-dosieron
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

RUN curl -LO https://github.com/revuloj/voko-iloj/archive/master.zip \
  && unzip master.zip voko-iloj-master/xsl/* voko-iloj-master/dtd/* voko-iloj-master/cfg/* \
  && rm master.zip 

# Nun ni kreos la propran procesumon por la redaktilo...
FROM swipl:stable

RUN apt-get update && apt-get install -y --no-install-recommends \
    xsltproc sqlite3 unzip && rm -rf /var/lib/apt/lists/* 
# ĉu jam enestas? libsqlite3-0 libsqlite3-dev

RUN swipl -g "pack_install(googleclient,[interactive(false)]),halt" -t "halt(1)"
# jam enestas en swipl:stable: RUN swipl -g "pack_install(prosqlite,[interactive(false)]),halt" -t "halt(1)"

RUN useradd -ms /bin/bash -u 1088 cetonio
USER cetonio:users
WORKDIR /home/cetonio

ADD . ./
#  ??? --chown=root:root
COPY --from=builder redaktilo-gen.js /home/cetonio/pro/web/
COPY --from=builder voko-iloj-master/ /home/cetonio/voko/

RUN mkdir -p tmp && mkdir -p sql  
  # && chmod 750 ./do*.sh

CMD ["swipl",\
    "-s","pro/redaktilo-servo.pl",\
    "-g","debug(http(_)),redaktilo_servo:daemon","-t","halt",\
    "-p","agordo=etc","--",\
    "--workers=10","--port=8080","--no-fork"]
