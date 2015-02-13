Just enough Postgres
====================

Understand the primary data-store of Zotonic.

Why
---

Data persistence in Zotonic is provided by PostgreSQL, a mature
feature-rich open-source relational database.

Since Zotonic provides both a data model and wrapper around PostgreSQL
queries, the Zotonic user is substantially insulated from routine
Postgres operation. But now and again, for issues ranging from
installation, backup/restore, and debugging, familiarity with a few
PostgreSQL commands can save considerable time.

Sadly, PostgreSQL user documentation is abundant, but not as well
organized as one might hope.

Assumptions
-----------

These commands have been tested on Debian Squeeze and Ubuntu 11.04.

Familiarity with SQL and relational databases advised.

NOTE: The string “VERSION,” as used below, refers to your PostgreSQL
version: E.g. 8.3 for Debian Lenny, 8.4 for Debian Squeeze and Ubuntu
11.04

How
---

**Where can I find PostgreSQL documentation?**

If PostreSQL is installed: :file:`/usr/share/doc/postgresql-common/README.Debian.gz`.

For on-line documentation: http://www.postgresql.org/docs/VERSION/static/

For instance, in Debian Lenny, look for: http://www.postgresql.org/docs/8.3/static/

**Is PostgreSQL installed?**

In a shell::

  ls /usr/lib | grep postgresql

You should see:

  postgresql

**What version?**

In the shell::

  ls -l /usr/lib/postgresql/

You should see::

  drwxr-xr-x 4 root root 4096 2011-06-23 14:13 VERSION

**How can I install PostgreSQL?**

In the shell::

  apt-get update
  apt-get install postgresql postgresql-client

Or, from source: http://www.postgresql.org/docs/8.4/static/install-short.html

**Where are Postresql files located?**

Configuration files: ``/etc/postgresql/[version]/[cluster]/``
Binaries: ``/usr/lib/postgresql/[version]``
Data files: ``/var/lib/postgresql/[version]/[cluster]``

**Is the PostgreSQL server running?**

In the shell::

  /etc/init.d/postgresql status

You should see something like::

  Running clusters: 8.4/main

**How can I stop the PostgreSQ server?**

In the shell::

  /etc/init.d/postgresql stop

You should see something like::

  * Stopping PostgreSQL 8.4 database server     [ OK  ]

**How can I start the Postgres server?**

In the shell::

  /etc/init.d/postgresql start

You should see something like::

  * Starting PostgreSQL 8.4 database server      [ OK ]

**How can I restart the PostgreSQL server?**

In the shell::
  
  /etc/init.d/postgresql restart

You should see something like::

  * Restarting PostgreSQL 8.4 database server

**How can I switch to database ‘zotonic_blog’ in psql?**

In the shell::

  zotonic@host $ psql
  zotonic=# \c zotonic_blog

You should now be on psql for the zotonic_blog database::

  You are now connected to database “zotonic_pcc”.
  zotonic_blog=#

**How can I enter the PostgreSQL interactive terminal?**

In the shell::

  psql

You should now be on the interactive terminal::

  psql (8.4.8)
  Type “help” for help.

  postgres=#

**How can I list databases?**

From psql::

  \l

Or directly from the Zotonic User’s shell::

  psql -l

You should see a list of databases like the following::

                                     List of databases
       Name     |  Owner   | Encoding |  Collation  |    Ctype    |   Access privileges   
  ———————+—————+—————+——————–+——————–+———————————–
   postgres     | postgres | UTF8     | en_US.UTF-8 | en_US.UTF-8 | 
   template0    | postgres | UTF8     | en_US.UTF-8 | en_US.UTF-8 | =c/postgres
                                                                  : postgres=CTc/postgres
   template1    | postgres | UTF8     | en_US.UTF-8 | en_US.UTF-8 | =c/postgres
                                                                  : postgres=CTc/postgres
   zotonic      | zotonic  | UTF8     | en_US.UTF-8 | en_US.UTF-8 | 
   zotonic_blog | zotonic  | UTF8     | en_US.UTF-8 | en_US.UTF-8 | 
  (5 rows)

**How can I see if a database exists?**

In the shell::

  psql test

If the database doesn’t exist::

  psql: FATAL: database “test” does not exist

If the database exists, you’ll see something like::

  psql (8.4.8)
  Type “help” for help.

  test=>

**How can I tell if the database for site ‘blog’ exists in the local postgres installation?**

In the shell::

  psql -l | grep blog

You should see something like::

  zotonic_blog | zotonic  | UTF8     | en_US.UTF-8 | en_US.UTF-8 | 

**How do I list the relations (tables, views , sequences) in a database?**

In the shell::

  psql zotonic_blog
  zotonic_blog=# \d

You should see something like::

                         List of relations
   Schema |               Name                |   Type   |  Owner
  ————+—————————————————–+—————+————–
   public | category                          | table    | zotonic
   public | comment                           | table    | zotonic
   public | comment_id_seq                    | sequence | zotonic
   public | config                            | table    | zotonic
   public | config_id_seq                     | sequence | zotonic
   public | edge                              | table    | zotonic
  {…etc. }

If psql displays this in a pager (prompt is a :) you can escape by hitting q.

**How can I create a table in a database?**

NOTE: Many fine books and tutorials are available to help you learn
SQL, the standard query language for relational databases. See
references below.

The follow queries are for illustration only::

  postgres=# CREATE TABLE books (
  postgres(# title text NOT NULL);
  CREATE TABLE

How to add a column to a table::

  postgres=# ALTER TABLE books
  postgres-# ADD author text NOT NULL;
  ALTER TABLE

How to examine the structure of a table::

  postgres=# \d books
     Table “public.books”
   Column | Type | Modifiers
  ————+———+—————–
   title  | text | not null
   author | text | not null

How to insert a record into a table::

  postgres=# INSERT INTO books ( title, author )
  postgres-# VALUES (‘Programming Erlang’, ‘Joe Armstrong’);
  INSERT 0 1

How to examine records in a table::

  postgres=# SELECT * FROM books;
  
         title        |    author    
  ——————————+———————–
   Programming Erlang | Joe Armstrong
  (1 row)

How to select a record from a table::

  postgres=# SELECT title FROM books
  postgres-# WHERE author = ‘Joe Armstrong’;
         title        
  ——————————
   Programming Erlang
  (1 row)

How to create a database user::

  postgres=# CREATE USER myuser WITH PASSWORD ‘userpassword’;
  CREATE ROLE

How to create a database::

  postgres=# CREATE DATABASE testdb WITH OWNER = myuser ENCODING = ‘UTF8’;
  CREATE DATABASE
  postgres=# GRANT ALL ON DATABASE testdb TO myuser;
  GRANT

How to initialize a database:

http://www.postgresql.org/docs/8.4/static/app-initdb.html

How can I back-up a database:

— Method 1: Use Backing up your site.

— Method 2: Dump can be created on the source machine with the following command (replace zotonic_blog with your site’s db name)::

  pg_dump zotonic_blog > zotonic_blog.sql

How to delete a database named ‘test’ and all its contents::

  pg_dump test > test.sql
  dropdb test

How can I restore the contents of a database from backup

See :ref:`cookbook-restore-db`

**Zotonic Conveniences that avoid direct Postgres interaction**

How can I create a database for my first Zotonic?::

  zotonic createdb blog
  zotonic addsite -d zotonic_blog blog

How can I create a database for an additional Zotonic site?::

  zotonic createdb blog
  zotonic addsite -d zotonic_blog blog

Notice the pattern ;)

**How can I open the Zotonic shell?**

In the terminal::

  zotonic shell

**How can I select records from the Zotonic shell?**

In the zotonic shell::
  
  1> m_rsc:get(page_home, z:c(blog)).
  [{category_id,104},
   {created,{{2011,6,8},{22,21,55}}},
   {creator_id,1},
   {id,313},
   {is_authoritative,true},
   {is_featured,false},
   {is_protected,false},
   {is_published,true},
   {modified,{{2011,6,8},{22,21,55}}},
   {modifier_id,1},
   {name,<<“page_home”>>},
   {page_path,<<“/”>>},
   {publication_end,{{9999,8,17},{12,0,0}}},
   {publication_start,{{2011,6,8},{22,21,55}}},
   {slug,<<“home”>>},
   {uri,undefined},
   {version,1},
   {visible_for,0},
   {title,<<“Home”>>},
   {summary,<<“Welcome to your blog!”>>},
   {managed_props,[{title,<<“Home”>>},
                   {summary,<<“Welcome ”…>>},
                   {page_path,<<“/”>>}]},
   {installed_by,z_install_defaultdata}]

Troubleshooting
---------------

Pay GREAT attention to permissions. Your tables and sequences should
be owned by the user specified in the site’s config file. GRANT may
not be enough. So, if you see Zotonic trying to recreate tables or if
Zotonic fails with a 3D000 error (database object doesn’t exist) even
if you are positive already exist, it means your permissions are
wrong.

**Problem:**

You try to get an psql shell::

  psql

And it refuses to work::

  psql: FATAL:  Ident authentication failed for user “postgres”

**Solution:**

You need to configure ``pg_hba.conf``

Note: For maximum security, correct configuration of pg_hba.conf is essential.

See :ref:`psql-trust-authentication` in this manual, or look at the Postgresql docs:

http://www.postgresql.org/docs/8.4/interactive/client-authentication.html
http://www.postgresql.org/docs/8.4/interactive/auth-pg-hba-conf.html

**Problem:**

In postgres, you get the following::
  
  testdb=> CREATE USER testdb WITH PASSWORD ‘testdb’;
  ERROR:  permission denied to create role

**Solution:**

You need to create a database user. Retry as the Postgres superuser::

  sudo su postgres psql

And it will work::

  postgres=# CREATE USER testdb  WITH PASSWORD ‘testb’;
  CREATE ROLE


**Problem:**

In the shell::

  cd /etc/postgresql

Outputs::
  bash: cd: /etc/postgresql: No such file or directory

**Solution:**
  
This is evidently a bug in certain Debian Lenny installs when
/etc/postgresql is inadvertently deleted. Uninstalling
postgresql-client (``apt-get —purge remove postgresql-client``) is
supposed to fix it. But it won’t if the system has an older version of
udev.

See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=517389

Need updated version of udev

**Problem:**

Erratic performance of database

**Solution:**

Examine PostgreSQL installation files. Expect trouble if, by
happenstance, you have more than one instance of PostgreSQL server
running. You may have to back-up your data, uninstall all PostgreSQL
files and reinstall.

Note: On some Lenny installations ``apt-get —purge remove postgresql``
will `NOT` remove all configuration files. And, ``apt-get install
posgtresql`` will not replace missing a missing ``/etc/postgresql``
directory and files.

Resources
---------

Howto: Debian / Ubuntu Linux Install PostgreSQL Database Server
http://www.cyberciti.biz/faq/linux-installing-postgresql-database-server/

psql: FATAL: Ident authentication failed for user “username” Error and Solution
http://www.cyberciti.biz/faq/psql-fatal-ident-authentication-failed-for-user/

PostgreSQL for Beginners
http://www.postgresqlforbeginners.com/2010/11/interacting-with-postgresql-psql.html

PostgreSQL 8.4.8 Documentation
http://www.postgresql.org/docs/8.4/static/index.html
http://www.postgresql.org/docs/8.4/static/reference-client.html

Howto Backup PostgreSQL Databases Server With pg_dump command
http://www.cyberciti.biz/tips/howto-backup-postgresql-databases.html

How To Use pg_dump and pg_restore with Postgres Plus® Tutorial for Linux®
http://www.enterprisedb.com/resources-community/tutorials-quickstarts/linux/how-use-pgdump-and-pgrestore-postgres-plus-tutorial-

An almost idiot’s guide to Install and Upgrade to PostgreSQL 8.4 with Yum
http://www.postgresonline.com/journal/archives/144-An-almost-idiots-guide-to-Install-and-Upgrade-to-PostgreSQL-8.4-with-Yum.html

postgresql clustering and Debian
http://www.progsoc.org/~wildfire/notes/postgresql-cluster.html

Books
-----

Momjian, Bruce, PostgreSQL: Introduction and Concepts, 2001, Addison-Wesley, Upper Saddle River, NJ, 462 pp

Worsley, John C. and Joshua D. Drake, Practical PostgreSQL, 2002, O'Rielly & Associates, Inc., Sebastopol, CA, 618 pp

