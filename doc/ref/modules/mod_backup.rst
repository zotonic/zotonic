
.. include:: meta-mod_backup.rst


mod_backup serves two different purposes: it makes a nightly backup of
your files and database, and can also backup/restore individual
:term:`resource` items.


Daily backup of database and files
----------------------------------

Losing data is bad for business.  This applies to your customers as
well if you are building sites for them.  It is critical to keep
backups of any Zotonic sites you develop.

After enabling mod_backup, it will make a backup of the site’s data
every night at 3 AM. It keeps the last 7 daily copies of the data, so
you have always a backup to roll back to.

The backups are stored under ``backup`` in the files directory of
your site. Check in the admin under System > Status to see where the
site files directory is located.

The site’s media files are stored as a ``.tar.gz`` file, while the
database is stored as an uncrompressed ``.sql`` file.

If :ref:`mod_filestore` is enabled then the media files are not backed up, as
it is assumed that the files are already backed up on the cloud filestore.
The data backups are uploaded to the cloud filestore, so they are also
backed up.

If :ref:`mod_filestore` is not enabled then we advise to add a
`cron <http://en.wikipedia.org/wiki/Cron>`_ script to the server for
copying the data to remote storage.

Per-resource revision log
-------------------------

If a resource is updated or deleted then a copy of the resource data
is saved to a revision log.

Using the revision log a resource can be rolled back to an older revision
or, when deleted, recovered.

Revisions older than 18 months are daily pruned. This can be changed by
setting the configuration ``mod_backup.revision_retention_months`` to another
number of months.

Currently edges (connections) and medium files are not kept in the revision log.
