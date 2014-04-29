
.. include:: meta-mod_filestore.rst

Support for storing uploaded and generated images and documents on external services.

Overview
--------

This module stores uploaded files and generated preview-images on an external S3-compatible service.
It listens for medium and file related notifications for any newly uploaded or generated files.

If a file is added then the file is queued in an upload queue. After a delay a separate process
polls this queue and will upload the file to the external service.

If a file is needed and not locally available then the mod_filestore module will check its 
file registry to see if the file is stored on an external service. If so then then a `filezcache`
process is added and a download of the file is started.

The file is served from the filezcache whilst it is being downloaded.

The `filezcache` will stop the entry after a random amount of time—if the entry was not recently 
used.


Configuration
-------------

After the `mod_filestore` is enabled an extra menu entry `Cloud File Store` is added
to the `System` menu in the admin.

Selecting the menu will show the configuration panel for the Could File Store.

.. image:: /img/filestore_admin.png

Here you can define where the files should be stored and give the credentials to access
the storage.

If you save the url and credentials then the system will try to upload a small file
to the remote storage. If it succeeds then the configuration is saved. If it does not
succeed then an error message will be displayed and the configuration will not be
changed.

It is possible to (temporarily) disable uploading new files by unchecking the checkbox *Upload new files to the cloud*.


Statistics
..........

The system shows statistics:

Media
   All `medium` records and a sum of the sizes. A single medium record can have 0, 1 or 2 files attached.

Local Files
   These are all files found in the `files` directory, this includes files that won’t ever be uploaded.

Cloud Files
   All files registered to be on any cloud service. This is extracted from the database and not by scanning
   the remote cloud service.

Queues
   These are the queues being processed by `mod_filestore`. On a quiet (stable) system they are usually empty.


Moving files
............

It is possible to move (almost) all files from the local file system to the cloud. And vice versa, from the cloud
to the local file system. This is useful when starting or changing the cloud storage location.

If a file is moved to the cloud then it is first placed in the filezcache. The filezcache will start purging the 
files if the cache is bigger than configurated in the filezcache application (default 10GB for all sites combined).

The system waits 10 minutes before a queued file is uploaded. This period is meant for a *cool down* of the file, as
in the first moments after an upload some resize and preview operations will take place. The delay makes it less
probable that a freshly uploaded file vanishes (to the cache) whilst a preview-generation is starting.


Notifications
-------------

The `mod_filestore` hooks into the following notifications, whose definitions can be found in ``zotonic_file.hrl``:


``#filestore{}``
    Hooks into the Zotonic file management notifications to upload, delete or lookup files. This will trigger downloads of
    external files and interfaces to the filezcache.

``#filestore_credentials_lookup{}``
    Maps a local path and optional resource id to a service, external location and key/password for that external service.
    This can be used to store different resources on different external services.

``#filestore_credentials_revlookup{}``
    Maps a cloud file service and location to a key, password and request location.

``#medium_update_done{}``
    Queues newly inserted medium files into the upload queue.

``#admin_menu{}``
    To add the `Cloud File Store` menu to the admin.



Applications
------------

The filestore uses the `s3filez` and `filezcache` Erlang applications.

s3filez
.......

This application is used for uploading, downloading and deleting files on S3 compatible services.
It provides asynchronous services and is compatible with the filezcache application.
It is also able to stream files to and from the external S3 service, this makes it possible to have start serving
a file before it is downloaded to the filezcache.

filezcache
..........

This application manages a cache of downloaded files. The cache is shared between all sites.
Every cache entry is managed by its own process, which can stream newly received data directly to any requesting
processes.

The filezcache keeps a presistent *disk_log* with a description of all files in the cache. This log is read on startup
to repopulate the cache with already present files. For each file the size and a hash is stored to check cache
consistency.

The filezcache has a garbage collector. It keeps a pool of randomly selected cache entries, from which it will 
elect randomly processes to be garbage-collected. The processes themselves will decide if they will stop or not.

After a cache process stops it will keep running for a short period to handle late incoming requests.

Filezcache entries are started by the `mod_filestore` and filled by either moving a local file to the cache
or by s3filez download processes.


.. todo:: The statistics are generated dynamically, which is not a good idea with many files. This will be changed.

.. seealso:: :ref:`model-filestore`.
