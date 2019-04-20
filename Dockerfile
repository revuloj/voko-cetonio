# Ni bezonas Javon kaj Closure-Compiler por kompili la Javo-skript-dosieron
FROM openjdk:jre-slim as builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl && rm -rf /var/lib/apt/lists/* 

ADD . ./

RUN curl -LO https://dl.google.com/closure-compiler/compiler-latest.tar.gz \
  && tar -xvzf compiler-latest.tar.gz 

RUN java -jar closure-compiler*.jar --dependency_mode LOOSE --js_module_root js_src --js js_src/*.js \
           --js_output_file redaktilo-gen.js #--entry_point ui_kreo.js 

RUN curl -LO https://github.com/revuloj/voko-iloj/archive/master.zip \
  && unzip master.zip voko-iloj-master/xsl/* voko-iloj-master/dtd/* voko-iloj-master/cfg/* \
  && rm master.zip && ls voko-iloj-master/* 

# Nun ni kreos la propran procesumon por la redaktilo...
FROM swipl:stable

RUN apt-get update && apt-get install -y --no-install-recommends \
    xsltproc && rm -rf /var/lib/apt/lists/* 

RUN swipl -g "pack_install(googleclient,[interactive(false)]),halt" -t "halt(1)"

RUN useradd -ms /bin/bash -u 1088 cetonio
USER cetonio:users
WORKDIR /home/cetonio

ADD . ./

COPY --from=builder redaktilo-gen.js /home/cetonio/pro/web/
COPY --from=builder voko-iloj-master/ /home/cetonio/voko/

USER root

CMD ["swipl",\
    "-s","pro/redaktilo-servo.pl",\
    "-g","daemon","-t","halt",\
    "-p","agordo=etc","--",\
    "--workers=10","--user=cetonio","--port=8081","--no-fork"]
