FROM ubuntu:16.04

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y git

####################### Install letsencrypt #######################

RUN echo "2019-11-13"
RUN git clone https://github.com/letsencrypt/letsencrypt && cd letsencrypt && ./letsencrypt-auto --help

####################### Copy files #######################

COPY scripts/renew.sh /renew.sh

####################### Default command #######################

CMD /renew.sh
