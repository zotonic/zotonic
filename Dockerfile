FROM debian
MAINTAINER Andreas Stenius git@astekk.se

RUN apt-get update && apt-get install -y erlang build-essential postgresql imagemagick exif wget git && \
    useradd --system --create-home zotonic && \
    printf "# Zotonic settings \n\
local   all         zotonic                           ident \n\
host    all         zotonic     127.0.0.1/32          md5 \n\
host    all         zotonic     ::1/128               md5" >> /etc/postgresql/9.4/main/pg_hba.conf && \
    /etc/init.d/postgresql start && \
    echo "CREATE USER zotonic WITH PASSWORD 'zotonic'; \
          ALTER ROLE zotonic WITH CREATEDB; \
          CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8'; \
          \c zotonic \
          CREATE LANGUAGE \"plpgsql\";" | su -l postgres -c psql

WORKDIR /home/zotonic
EXPOSE 8000
ENV ERL_FLAGS -noinput
ENTRYPOINT ["./bin/zotonic"]
CMD ["debug"]
VOLUME /etc/zotonic

ADD . /home/zotonic/
RUN mkdir -p /etc/zotonic/config /etc/zotonic/user && \
    ln -s /etc/zotonic/config /home/zotonic/.zotonic && \
    ln -s /etc/zotonic/user /home/zotonic/user && \
    chown -LR zotonic:zotonic .

USER zotonic
RUN make
