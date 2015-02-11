# spreadsheetkit/monitor:1.1

FROM spreadsheetkit/base:1
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

# Install java
RUN apt-get update
RUN apt-get -y install openjdk-7-jdk

# Install logstash
RUN apt-get -y install wget
RUN wget -O - http://packages.elasticsearch.org/GPG-KEY-elasticsearch | apt-key add -
RUN sh -c "echo 'deb http://packages.elasticsearch.org/logstash/1.4/debian stable main' > /etc/apt/sources.list.d/logstash.list"
RUN apt-get update
RUN apt-get install logstash

# Install redis (logstash broker)
RUN apt-get -y install redis-server

# Install elasticsearch
RUN wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.4.2.deb
ENV JAVA_HOME /usr/lib/jvm/java-7-openjdk-amd64
RUN dpkg -i elasticsearch-1.4.2.deb
COPY elasticsearch.yml /etc/elasticsearch/

# Install curl
RUN apt-get -y install curl

# Add logstash config
COPY central.conf /etc/logstash/conf.d/central.conf

# Add syslog-ng config
COPY syslog-ng.conf /etc/syslog-ng/syslog-ng.conf

# runit: syslog-ng
RUN mkdir -p /etc/service/syslog-ng
COPY ./runit/syslog-ng/run /etc/service/syslog-ng/run

# runit: elasticsearch
RUN mkdir -p /etc/service/elasticsearch
COPY ./runit/elasticsearch/run /etc/service/elasticsearch/run

# runit: logstash
RUN mkdir -p /etc/service/logstash
COPY ./runit/logstash/run /etc/service/logstash/run

# runit: kibana
RUN mkdir -p /etc/service/kibana
COPY ./runit/kibana/run /etc/service/kibana/run


