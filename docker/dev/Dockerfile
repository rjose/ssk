# sskit/dev:1

FROM sskit/base:1
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

# Install libc dev
RUN sh -c "echo 'deb http://ftp.debian.org/debian sid main' > /etc/apt/sources.list.d/libc.list"
RUN apt-get update
RUN apt-get -t sid install -y libc6-dev

# Configure syslog-ng and add to runit
COPY ./to_copy/etc/syslog-ng.conf /etc/syslog-ng/syslog-ng.conf
