
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
every night at 3 AM. It keeps the last 10 copies of the data, so you
have alway a backup to roll back to.

The backups are stored under ``user/sites/yoursite/files/backup/``.

The site’s media files are stored as a ``.tar.gz`` file, while the
database is stored as an uncrompressed ``.sql`` file.

We advise to add a `cron <http://en.wikipedia.org/wiki/Cron>`_ script
to the server so the backup data is copied off of it on a regular
interval.

Per-resource revision log
-------------------------

If a resource is updated or deleted then a copy of the resource data
is saved to a revision log.

Using the revision log a resource can be rolled back to an older revision
or, when deleted, recovered.

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

Currently edges (connections) and medium files are not kept in the revision log.
