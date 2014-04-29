
.. include:: meta-filestore.rst

The filestore uses two tables for its administration.

Main administration table
-------------------------

The ``filestore`` table administrates which file is stored at what service.
It also contains flags for flagging files that need to be deleted or moved from the
remote service to the local file system.

.. highlight:: sql

The definition is as follows::

    create table filestore (
        id serial not null,
        is_deleted boolean not null default false,
        is_move_to_local boolean not null default false,
        error character varying(32),
        path character varying(255) not null,
        service character varying(16) not null,
        location character varying(400) not null,
        size int not null default 0,
        modified timestamp with time zone not null default now(),
        created timestamp with time zone not null default now(),
        
        constraint filestore_pkey primary key (id),
        constraint filestore_path_key unique (path),
        constraint filestore_location_key unique (location)
    )

The `path` is the local path, relative to the site’s ``files`` directory.
For example: ``archive/2008/12/10/tycho.jpg``

The `service` describes what kind of service or protocol is used for the remote
storage. Currently the service is always set to ``s3``.

The `location` is the full url describing the location of the file on the
remote service.
For example: ``https://s.greenqloud.com/mworrell-zotonic-test/archive/2008/12/10/tycho.jpg``

Upload queue
------------

The upload queue holds file paths of newly added files that need to be uploaded.
These file paths are relative to the ``files`` directory.

Periodically the system polls this table to see if any files need uploading.
Any entry older than 10 minutes will be handled and an upload process will be started.
