FROM debian
MAINTAINER Andreas Stenius git@astekk.se

ENV DEBIAN_FRONTEND noninteractive

# httpredir.debian.org/debian fails too much, so replace it with a fixed mirror.
RUN echo \
   'deb ftp://ftp.nl.debian.org/debian/ jessie main\n \
    deb ftp://ftp.nl.debian.org/debian/ jessie-updates main\n \
    deb http://security.debian.org jessie/updates main\n' \
    > /etc/apt/sources.list

ADD https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb .

RUN apt-get clean \
    && dpkg -i erlang-solutions_1.0_all.deb \
    && apt-get update \
    && apt-get install -y --no-install-recommends erlang build-essential ca-certificates imagemagick inotify-tools libnotify-bin wget git \
    && rm -rf /var/lib/apt/lists/*

ADD . /opt/zotonic
WORKDIR /opt/zotonic

RUN DEBUG=1 make

COPY docker/docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
ENV PATH /opt/zotonic/bin:$PATH

CMD ["debug"]

EXPOSE 8000
VOLUME /etc/zotonic
VOLUME /opt/zotonic/user/sites
VOLUME /opt/zotonic/user/modules
