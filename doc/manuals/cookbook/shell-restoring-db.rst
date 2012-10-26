Restore/upgrade content db from backup
======================================
`Contributed by: Scott Finnie`


How to protect your Zotonic content and bring it back in case of disaster.

Why
---
Restore / upgrade the content for a site from a database dump.

Assumptions
-----------

You have a dump of the content to restore as a ``.sql`` file, which
was created by :ref:`mod_backup`.

How
---

Dumps from postgres include both database structure (tables,
sequences, constraints, etc.) as well as content. Therefore it's not
possible to simply import the dump directly into an existing Zotonic
database populated with content. The target database must be empty
before the dump can be imported. There are (at least) 2 ways to do
this:

1. Drop contents of the target db and import the dump file;

2. Create a new database, import contents, rename old database to
   something temporary, rename new database to match site, and finally
   delete the old database.

Option 2 seems more involved but is safer and quicker: It's
non-destructive. The old db remains intact until after the new one is
activated. If anything goes wrong, you can fall back to the original.

It also means less downtime. The site can stay live on the old db
while the new db is being created. Downtime is restricted to the
rename operations (which are quick and db size independent).

For small databases the downtime difference will be minimal. However
the safety is appealing. Hence this cookbook uses the second approach.

Instructions
------------

These assume the following naming conventions:

- Your Zotonic site is named yoursite.
- The postgres dump file to load is named zotonic-dump.sql.
- Zotonic is installed in ~zotonic/zotonic

Replace these assumptions as appropriate in all commands below.
Ensure the db dump file is available on target machine (see
pre-requisites).

Create a new database in postgres::

  zotonic:~$ sudo su -l postgres
  postgres:~$ ~Zotonic/zotonic/bin/zotonic createdb yoursite_new
  CREATE DATABASE
  GRANT
  You are now connected to database "zotonic_yoursite_new".
  CREATE LANGUAGE
  postgres:~$

Import the dump file into the newly created db::

  postgres:~$ psql -U zotonic -d zotonic_yoursite_new -f /path/to/zotonic-dump.sql
  […lots of SQL statements…]
  postgres:~$ 

Note: the final sql commands shown will likely say 'no privileges could be revoked for "public"'. You can ignore this.

Stop Zotonic (if running)::

  postgres:~$ ~zotonic/zotonic/bin/zotonic stop 
  Stopping zotonic zotonic001@hostname
  Stop:'zotonic001@hostname'
  postgres:~$

Rename the databases::

  postgres:~$ psql
  postgres=# ALTER DATABASE zotonic_yoursite RENAME TO zotonic_yoursite_old; 
  ALTER DATABASE
  postgres=# ALTER DATABASE zotonic_yoursite_new RENAME TO zotonic_yoursite; 
  ALTER DATABASE
  postgres=# \q
  postgres:~$ exit
  zotonic:~$ 

Restart Zotonic::

  zotonic:~$ ~/zotonic/bin/zotonic start

Browse to your site & test it's now serving updated content.

(Optional) Drop the old database::

  zotonic:~$ sudo -u postgres psql
  postgres=# DROP DATABASE zotonic_yoursite_old;
  DROP DATABASE
  postgres=# \q
  zotonic:~$  
