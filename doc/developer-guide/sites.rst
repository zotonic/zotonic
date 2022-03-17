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

.. note:: If anything goes wrong, see the :ref:`ref-troubleshooting-installation`.

First, prepare the database. In your terminal, connect to PostgreSQL:

.. code-block:: bash

    $ sudo -u postgres psql (enter your OS password)

And create a database user for Zotonic. You may want to change the password::

    postgres=# CREATE USER zotonic WITH PASSWORD 'zotonic';

Now, either give this user create rights to have Zotonic automatically create
the database for you (recommended)::

    postgres=# ALTER USER zotonic CREATEDB;

Or create the site database manually::

    postgres=# CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
    postgres=# GRANT ALL ON DATABASE zotonic TO zotonic;

And quit postgres::

    postgres=# \q

Now that there is a database Zotonic can be started. We do this in debug mode
so that all console output is visible:

.. code-block:: bash

     $ bin/zotonic debug

In a new terminal window, edit your ``/etc/hosts`` file, adding an entry for ``yoursite.test`` (the
site hostname) to point at your local host:

.. code-block:: none

     127.0.0.1   yoursite.test

.. note:: Zotonic has to be running for the ``addsite`` command to succeed.

Create a new Zotonic site, based on the ‘blog’ skeleton site:

.. code-block:: bash

     $ bin/zotonic addsite -s blog yoursite

Finally, point your browser to https://yoursite.test:8443 to see your new site.
The browser will ask to accept a self-signed certificate. Zotonic generates a
self-signed certificate for every site. These are stored in :file:`~/.config/zotonic/security`
(on macOS :file:`~/Library/Application Support/zotonic/security`).

You can log into the admin at https://yoursite.test:8443/admin using the username ``admin``
with the password that you can find in your site’s configuration. Use for this the command:

.. code-block:: bash

      $ bin/zotonic siteconfig yoursite

The configuration is stored in the file :file:`apps_user/yoursite/priv/zotonic_site.config`
in the :term:`zotonic user directory`.

You can stop Zotonic by typing twice Ctrl-C at the Erlang command prompt.

If you want to start Zotonic in the background then use:

.. code-block:: bash

  $ bin/zotonic start

This can be stopped with:

.. code-block:: bash

  $ bin/zotonic stop


.. _guide-site-anatomy:

Anatomy of a site
-----------------

A Zotonic site is a folder which lives in the :term:`zotonic user directory` and
contains at least:

* a ``priv/zotonic_site.config`` file: sets the site’s hostname and other parameters
* a ``src/sitename.erl`` file: initialises the site.
* a ``src/sitename.app.src`` file: an OTP app source file

In fact, a site is a special type of :ref:`module <guide-modules>`.
Like modules, sites usually contain additional resources such as
:ref:`templates <guide-templates>`,
:ref:`dispatch rules <guide-dispatch>` and
:ref:`data <guide-modules-versioning>` . Unlike modules, however, sites have
their own hostname and database connection.

Next steps
----------

* Consult the reference for all site :ref:`configuration parameters <ref-site-configuration>`.
* If something goes wrong, consult the :ref:`troubleshooting reference <ref-troubleshooting-sites>`.
