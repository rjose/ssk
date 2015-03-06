# sskit/pass3:1

FROM sskit/dev:1
MAINTAINER Rino Jose "rjose@spreadsheetkit.com"


# Copy release over and untar it
RUN mkdir /releases
COPY ./to_copy/releases/pass3-0.0.1.tar.gz /releases/
RUN cd /releases && tar -zxvf pass3-0.0.1.tar.gz

# TODO: Load local config files

# Add pass3 to runit
RUN mkdir -p /etc/service/pass3
COPY ./to_copy/etc/service/pass3/run /etc/service/pass3/run

