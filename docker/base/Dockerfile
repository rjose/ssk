# sskit/base:1

FROM debian:7.8
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"

# Refresh package list
RUN apt-get update

# Install wget
RUN apt-get install -y wget

# Add erlang solutions repo
RUN wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && dpkg -i erlang-solutions_1.0_all.deb

# Refresh package list
RUN apt-get update

# Install elixir
RUN apt-get install -y elixir

# Install runit
RUN apt-get install -y runit

# Bootstrap the environment variables
COPY ./to_copy/usr/sbin/runit_bootstrap /usr/sbin/runit_bootstrap

# Install syslog-ng
RUN apt-get install -y syslog-ng

# Update syslog-ng config to uncomment "SYSLOGNG_OPTS" line
# (This prevents an error message)
RUN sed -i 's/^#\(SYSLOGNG_OPTS="--no-caps"\)/\1/g' /etc/default/syslog-ng

# Add syslog-ng to runit
RUN mkdir -p /etc/service/syslog-ng/
COPY ./to_copy/etc/service/syslog-ng/run /etc/service/syslog-ng/run

# Start runit
ENTRYPOINT ["/usr/sbin/runit_bootstrap"]
