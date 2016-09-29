.. highlight:: bash

.. _sites:

Sites
=====

Zotonic has the capability of serving more than one site at a time. You can have
multiple sites enabled, each with its own set of templates, database and
dispatch rules. Each site has its own hostname.

.. _guide-create-site:

Create a site
-------------

1. First, prepare the database. In your terminal, connect to PostgreSQL::

    $ sudo -u postgres psql (enter your OS password)

   And create a database user for Zotonic. You may want to change the password::

    postgres=# CREATE USER zotonic WITH PASSWORD 'zotonic';

   Now, either give this user create rights to have Zotonic automatically create
   the database for you (recommended)::

    postgres=# ALTER USER zotonic CREATEDB;

   Or create the site database manually::

    postgres=# CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
    postgres=# GRANT ALL ON DATABASE zotonic TO zotonic;
    postgres=# \c zotonic
    postgres=# CREATE LANGUAGE "plpgsql";

2. Edit your ``/etc/hosts`` file, adding an entry for ``yoursite.dev`` (the
   site hostname) to point at your local host::

     127.0.0.1   yoursite.dev

3. Finally, create a new Zotonic site, based on the ‘blog’ skeleton site::

     $ bin/zotonic addsite -s blog yoursite

   .. note:: Zotonic has to be running for the ``addsite`` command to succeed.

4. Then rebuild Zotonic::

    $ cd dir/to/zotonic
    $ make

5. And (re)start Zotonic::

    $ bin/zotonic debug

5. Finally, point your browser to http://yoursite:8000 to see your new site.
   You can log into the admin at http://yoursite:8000/admin with the password
   that you can find in your site’s configuration file: ``yoursite/config`` in
   the :term:`user sites directory`.

   .. note:: If anything goes wrong, see the :ref:`ref-troubleshooting-installation`.

.. _guide-site-anatomy:

Anatomy of a site
-----------------

A Zotonic site is a folder which lives in the :term:`user sites directory` and
contains at least:

* a ``config`` file: sets the site’s hostname and other parameters
* a ``sitename.erl`` file: initialises the site.

In fact, a site is a special type of :ref:`module <guide-modules>`.
Like modules, sites usually contain additional resources such as templates and
dispatch rules. Unlike modules, however, sites have their own hostname and
database connection.


Next steps
----------

* Consult the reference for all site :ref:`configuration parameters <ref-site-configuration>`.
* If something goes wrong, consult the :ref:`troubleshooting reference <ref-troubleshooting-sites>`.

