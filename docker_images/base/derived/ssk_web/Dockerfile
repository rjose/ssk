# spreadsheetkit/ssk_web:1.1

FROM spreadsheetkit/base:1
MAINTAINER Rino Jose "rjose@linkedin.com"

# Add release files to "/app"
ADD rel /app 


# runit: ssk_web
RUN mkdir -p /etc/service/ssk_web/
COPY ./runit/ssk_web/run /etc/service/ssk_web/run

#COPY syslog-ng.conf /etc/syslog-ng/syslog-ng.conf

# runit: syslog-ng
#RUN mkdir -p /etc/service/syslog-ng/
#COPY ./runit/syslog-ng/run /etc/service/syslog-ng/run
