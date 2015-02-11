# spreadsheetkit/dev:1.1

FROM spreadsheetkit/base:1
MAINTAINER Rino Jose "rjose@linkedin.com"


COPY syslog-ng.conf /etc/syslog-ng/syslog-ng.conf

# runit: syslog-ng
RUN mkdir -p /etc/service/syslog-ng/
COPY ./runit/syslog-ng/run /etc/service/syslog-ng/run
