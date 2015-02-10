# spreadsheetkit/base:1

FROM debian:8.0
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

RUN apt-get update

# Install erlang
RUN apt-get install -y erlang-base
RUN apt-get install -y erlang-inets
RUN apt-get install -y erlang-lager

# Install runit
RUN apt-get install -y runit

# Bootstrap the environment variables
COPY runit_bootstrap /usr/sbin/runit_bootstrap

# Add sample
RUN mkdir -p /etc/service/hello/
COPY ./hello/run /etc/service/hello/run

ENTRYPOINT ["/usr/sbin/runit_bootstrap"]