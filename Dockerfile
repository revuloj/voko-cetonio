FROM swipl:stable

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl unzip xsltproc \
        && rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash -u 1088 cetonio
USER cetonio:users
WORKDIR /home/cetonio

ADD . ./

RUN curl -LO https://github.com/revuloj/voko-iloj/archive/master.zip \
  && unzip master.zip voko-iloj-master/xsl/ voko-iloj-master/dtd/ voko-iloj-master/cfg/ \
  && rm master.zip && ln -s voko-iloj-master/xsl xsl \
  && ln -s voko-iloj-master/dtd dtd && ln -s voko-iloj-master/cfg cfg

USER root

CMD ["swipl",\
    "-s","pro/redaktilo-servo.pl",\
    "-g","daemon","-t","halt",\
    "-p","agordo=etc","--",\
    "--workers=10","--user=cetonio","--port=8081","--no-fork"]
