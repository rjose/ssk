# sskit/monitor:1

FROM sskit/base:1
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

# Install curl
RUN apt-get -y install curl

# Configure syslog-ng
COPY ./to_copy/etc/syslog-ng.conf /etc/syslog-ng/syslog-ng.conf
 
# Configure elasticsearch and add to runit
COPY ./to_copy/etc/elasticsearch/elasticsearch.yml /etc/elasticsearch/
RUN mkdir -p /etc/service/elasticsearch
COPY ./to_copy/etc/service/elasticsearch/run /etc/service/elasticsearch/run

# Configure logstash and add to runit
COPY ./to_copy/etc/logstash/conf.d/central.conf /etc/logstash/conf.d/central.conf
RUN mkdir -p /etc/service/logstash
COPY ./to_copy/etc/service/logstash/run /etc/service/logstash/run
 
# Configure kibana and add to runit
RUN mkdir -p /etc/service/kibana
COPY ./to_copy/etc/service/kibana/run /etc/service/kibana/run

# Create a /working directory so we can have the log file persist
RUN mkdir /working
RUN cd /var/log && ln -s /working/messages .
