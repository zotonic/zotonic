.. highlight:: sh
.. _installation-preinstall:

Installation requirements
=========================

Zotonic runs on Linux, Mac OS X and (not officially) on Windows.

Before running Zotonic, you must make sure your system meets the
minimum requirements to do so. Zotonic needs the following software
installed:

1. Erlang/OTP version **18** or newer. Build it from source, or use
   packages.

2. **ImageMagick** (version 6.5 or higher) for the ``convert`` and
   ``identify`` tools.  Make sure that the convert and identify tools
   are in your path so that zotonic can find them. For auto-rotation
   to work you'll need the ``exif`` utility as well.

3. **PostgreSQL** version 8.4 or higher. Enable trust-authentication
   (username+password) in Postgres (see below).

4. **gcc** and **g++** For compiling dependencies like `erlexec`.

5. **git** Zotonic comes with a few subprojects which are pulled from
   the web with the ``git`` command.

6. **gettext** For translation support.

If you meet these requirements, head straight on to :ref:`guide-installation`,
otherwise, read on below for the specifics on these.

Erlang
------

You can check if you have Erlang installed by typing the following
command in a terminal::

  $ erl

The output should be something like::

  Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
  Eshell V8.3  (abort with ^G)
  1>

(Press ctrl+c twice to exit)

If your version is below release **18**, you need to upgrade. If
you don't have Erlang installed, we recommend downloading a build for
your operating system from the Erlang Solutions website:

https://www.erlang-solutions.com/downloads/download-erlang-otp


ImageMagick
-----------

You can check if you have ImageMagick installed by typing the following
command in a terminal::

  $ convert -version

This tells you which version of ImageMagick is installed::

  Version: ImageMagick 6.8.9-9 Q16 x86_64 2018-09-28 http://www.imagemagick.org


PostgreSQL
----------

You can check if you have PostgreSQL installed by typing the following
command in a terminal::

  $ psql -V

Returns the PostgreSQL version::

  psql (PostgreSQL) 9.5.17


.. _psql-trust-authentication:

Enabling trust-authentication in PostgreSQL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To let Postgres users access the database from Zotonic, you need to
make a configuration change in Postgres' configuration file ``pg_hba.conf``.

On Linux, this file is located in ``/etc/postgresql/<pg
version>/main/pg_hba.conf``. Add the following lines::

  # Zotonic settings
  local   all         zotonic                           ident
  host    all         zotonic     127.0.0.1/32          md5
  host    all         zotonic     ::1/128               md5

These settings assume that your Zotonic sites are going to connect
with a PostgreSQL user called ``zotonic``. For other user names, adjust
accordingly. Do not forget to restart PostgreSQL after you've made this
change.


Platform-specific notes
-----------------------

Ubuntu / Debian
^^^^^^^^^^^^^^^

We recommend you install Erlang from the Erlang solutions website:

https://www.erlang-solutions.com/downloads/download-erlang-otp

The other requirements are easily fetched with ``apt``::

  sudo sudo apt-get install gcc g++ build-essential git imagemagick postgresql


FreeBSD
^^^^^^^

If you’re running on FreeBSD, make sure you've got the 'GNU' 'make'
(check with ``make --version``, which should give you GNU, and version
info). If you don't have GNU make, Zotonic will give an error when
trying to compile.


MacOS
^^^^^

With Homebrew you can install Erlang and ImageMagick using the
following commands::

  brew install erlang git imagemagick

Alternatively, with MacPorts::

  sudo port install erlang +ssl
  sudo port install ImageMagick

For PostgreSQL choose either:

* `EnterpriseDB <https://www.enterprisedb.com/products/pgdownload.do>`_
* `Postgress.app <https://postgresapp.com/>`_


Windows
^^^^^^^

Currently, Zotonic is not officially supported on the Windows
platform. However, the main dependencies Erlang, PostgreSQL and
ImageMagick do work on Windows, so, if you’re adventurous, it should
be possible to get it running.
