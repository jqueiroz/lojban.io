FROM ubuntu:16.04

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y libgmp-dev netbase

####################### Copy files #######################
COPY resources /lojto/resources
COPY static /lojto/static
COPY .docker-binary /lojto/lojto

####################### Expose ports #######################
EXPOSE 8000/tcp

####################### Default command #######################
CMD cd /lojto && /lojto/lojto
