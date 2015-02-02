# spreadsheetkit/monitor:1.1

FROM spreadsheetkit/base:1
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

# Install java
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

# Add logstash config
ADD central.conf /etc/logstash/conf.d/central.conf


WORKDIR /monitor
