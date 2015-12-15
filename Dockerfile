FROM debian
MAINTAINER Andreas Stenius git@astekk.se

RUN apt-get update                                                                                 && \
    apt-get install -y erlang build-essential postgresql imagemagick exif wget git                 && \
    useradd --system --create-home zotonic                                                         && \
    printf "# Zotonic settings \n\
local   all         zotonic                           ident \n\
host    all         zotonic     127.0.0.1/32          md5 \n\
host    all         zotonic     ::1/128               md5" >> /etc/postgresql/9.4/main/pg_hba.conf && \
    /etc/init.d/postgresql start                                                                   && \
    echo "CREATE USER zotonic WITH PASSWORD 'zotonic'; \
          ALTER ROLE zotonic WITH CREATEDB; \
          CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8'; \
          \c zotonic \
          CREATE LANGUAGE \"plpgsql\";" | su -l postgres -c psql

EXPOSE 8000
ENV ERL_FLAGS -noinput
ENTRYPOINT ["./bin/zotonic"]
CMD ["debug"]

WORKDIR /home/zotonic
ADD . /home/zotonic/

RUN make                                      && \
    su -l zotonic -c 'bin/zotonic configfile' && \
    mkdir /etc/zotonic                        && \
    mv .zotonic /etc/zotonic/config           && \
    mv user /etc/zotonic/user                 && \
    chown -R zotonic:zotonic /home/zotonic /etc/zotonic

USER zotonic
VOLUME /etc/zotonic
