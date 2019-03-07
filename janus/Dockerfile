############################################################
# Dockerfile - Janus Gateway on Debian Jessie
# https://github.com/krull/docker-janus
############################################################

# set base image debian jessie
FROM debian:stretch-slim

# file maintainer author
MAINTAINER Amin Arria <arria.amin@gmail.com>

WORKDIR /root

RUN apt update

# Install needed utils
RUN apt install -y git make

# Install all dependencies
RUN apt install -y \
      libmicrohttpd-dev libjansson-dev \
      libssl-dev libsofia-sip-ua-dev libglib2.0-dev \
      libopus-dev libogg-dev libcurl4-openssl-dev liblua5.3-dev \
      libconfig-dev pkg-config gengetopt libtool automake \
      libnice-dev libsrtp2-dev libwebsockets-dev

RUN git clone https://github.com/sctplab/usrsctp \
    && cd usrsctp \
    && ./bootstrap \
    && ./configure \
    && make \
    && make install

# Compile Janus
RUN git clone https://github.com/meetecho/janus-gateway.git \
    && cd janus-gateway \
    && sh autogen.sh \
    && ./configure --prefix=/opt/janus --disable-rabbitmq --disable-mqtt \
    && make \
    && make install \
    && make configs

ENV LD_LIBRARY_PATH /usr/local/lib

RUN apt install -y coturn \
    && turnadmin -a -u bob -p 1234 -r usa \
    && turnserver -o

CMD ["/opt/janus/bin/janus"]
