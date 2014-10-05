FROM ubuntu:14.04
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

RUN apt-get update

RUN apt-get install -y wget

RUN mkdir build && cd build && wget http://www.erlang.org/download/otp_src_17.3.tar.gz

RUN cd build && tar -zxvf otp_src_17.3.tar.gz

ENV ERL_TOP /build/otp_src_17.3

RUN apt-get install -y libssl-dev
RUN apt-get install -y build-essential
RUN apt-get install -y ncurses-dev

RUN cd build/otp_src_17.3 &&  ./configure && make && make install