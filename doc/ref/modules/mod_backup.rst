
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
and configuration every night at 3 AM. It keeps the last 7 daily
copies of the data and a weekly backup for the last 4 weeks, so you
have always a backup to roll back to.

The backups are stored under ``backup`` in the files directory of
your site. Check in the admin under System > Status to see where the
site files directory is located.

The site’s media files are stored as a ``<site-name>-N.tar.gz`` file. The
configuration is named ``config-<site-name>-N.tar.gz``, while the
database is stored compressed in a ``<site-name>-N.sql.gz`` file. N is
the day number. Sunday (day 1) is stored as the weekly backup, which rotates
over w1..w4.

It is possible to encrypt the backups by enabeling the Encrypt Backups 
option on the configuration page of the backup service. When you enable
this for the first time, a password will be generated automatically. 
This password should be stored in a safe location to be able to 
decrypt the backups. When encryption is enabled, the files have
the ``.enc`` extension. 

The zotonic shell command ``decrypt`` can be used to decrypt encrypted
zotonic backup files.

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

Edges are added to the revision log, and are replayed in reverse chronological order to
recover the connections of a resource.

If a connection is re-instantiated and it is referring to a *dependent* resource that is now
deleted, then that *dependent* resource will also be recovered.

Medium files are kept for 4 weeks before they are deleted. That means that up to four weeks a medium
file can be recovered, and after that it will be lost. Unless the file is still available on the
filestore (which has its own deletion interval).

Revisions are pruned daily and deleted if:

1. older than 18 months;
   This can be changed by setting the configuration ``mod_backup.revision_retention_months``
   to another number of months.
2. of user resources and older than 90 days;
   This can be changed by setting the configuration ``mod_backup.user_revision_retention_days``
   to another number of days.
3. of users resources and the users was deleted for at least 30 days;
   This can be changed by setting the configuration ``mod_backup.user_deletion_retention_days``
   to another number of days (maximum 30).

Replication and failover
------------------------

It is possible to replicate data from one server to another server. The receiving server
must have the site running with its environment set to ``backup``.

If a site’s environment is set to ``backup`` then the site will start in a special mode. In backup
mode only essential modules, ``mod_filestore`` and ``mod_backup`` are started.

If there is a working filestore configuration then the backup module will start polling for
new backups uploaded to the filestore. Any newly found backups are downloaded to the local
file system.  After all backup files are downloaded, then for the newest backup:

  * the database backup will be imported; and
  * the configuration files in the ``priv/`` and ``priv/config.d/`` overwritten with the
    config files from the backup; and
  * certificates and secrets are written to the site’s security directory; and
  * any files backup tar will be unpacked in the local files directory; and finally
  * a file ``priv/BACKUP`` will be written.

After this the site will restart.

Because the file ``priv/BACKUP`` is present the environment of the site will be forced to ``backup``
and the site will start checking again for a new backup.

If a failover is needed, then change the DNS to point to the new server, remove the ``priv/BACKUP``
file and restart the site to start the site normally.

