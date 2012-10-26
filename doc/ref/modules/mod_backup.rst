
.. include:: meta-mod_backup.rst


mod_backup serves two different purposes: it makes a nightly backup of
your files and database, and can also backup/restore individual
:term:`resource` items.


Daily backup of database and files
----------------------------------
Losing data is bad for business.  This applies to your customers as
well if you are building sites for them.  It is critical to keep
backups of any Zotonic sites you develop.

After enabling mod_backup, it will make a backup of the site's data
every night at 3 AM. It keeps the last 10 copies of the data, so you
have alway a backup to roll back to.

The backups are stored under ``priv/sites/yoursite/files/backup/``.

The site's media files are stored as a ``.tar.gz`` file, while the
database is stored as an uncrompressed ``.sql`` file.

We advise to add a `cron <http://en.wikipedia.org/wiki/Cron>`_ script
to the server so the backup data is copied off of it on a regular
interval.

Per-resource backup/restore
---------------------------
.. todo:: Not yet documented.
