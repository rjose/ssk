# sskit/dev:1

FROM sskit/base:1
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

# Configure syslog-ng and add to runit
COPY ./to_copy/etc/syslog-ng.conf /etc/syslog-ng/syslog-ng.conf
RUN mkdir -p /etc/service/syslog-ng/
COPY ./to_copy/etc/service/syslog-ng/run /etc/service/syslog-ng/run
