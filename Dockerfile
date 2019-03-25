FROM ubuntu:18.04

# Error: Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp)

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y libgmp-dev netbase libgnutls30 libnss3 libnss3-dev ca-certificates

####################### Copy files #######################
COPY resources /lojto/resources
COPY static /lojto/static
COPY .docker-binary /lojto/lojto

####################### Expose ports #######################
EXPOSE 8000/tcp

####################### Default command #######################
CMD cd /lojto && /lojto/lojto
