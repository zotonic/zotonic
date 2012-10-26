.. highlight:: sh
.. _install-requirements:

Installation prerequisites
==========================

Before running Zotonic, you must make sure your system meets the
minimum requirements to do so. Zotonic needs the following software
installed:

1. **Erlang R14B03** or newer. Build it from source, or use
   packages.    

2. **ImageMagick** (version 6.5 or higher) for the ``convert`` and
   ``identify`` tools.  Make sure that the convert and identify tools
   are in your path so that zotonic can find them. For auto-rotation
   to work you'll need the ``exif`` utility as well.

3. **PostgreSQL** version 8.4 or higher. Enable trust-authentication
   (username+password) in Postgres (see below).

4. **make** A recent version of the GNU ``make`` utility.
      
5. **git** if you are planning to use the latest-and-greatest
   development version of Zotonic.

If you meet these requirements, head straight on to :ref:`tutorial-install`,
otherwise, read on below for the specifics on these.
   
Erlang
------

You can check if you have Erlang installed by typing the following
command in a terminal::

  $ erl

The output should be something like::

  Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
  Eshell V5.9.2  (abort with ^G)
  1>

(Press ctrl+c twice to exit)

If your version is below release **R14B03**, you need to upgrade.


ImageMagick
-----------

You can check if you have ImageMagick installed by typing the following
command in a terminal::

  $ convert -version

This tells you which version of ImageMagick is installed::

  Version: ImageMagick 6.7.7-10 2012-08-17 Q16 http://www.imagemagick.org


PostgreSQL
----------  

You can check if you have ImageMagick installed by typing the following
command in a terminal::

  $ psql -V

Returns the PostgreSQL version::

  psql (PostgreSQL) 9.1.6


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
with a Postgres user called ``zotonic``. For other user names, adjust
accordingly. Do not forget to restart Postgres after you've made this
change.


Platform-specific notes
-----------------------

Ubuntu ( >= 10.04)
^^^^^^^^^^^^^^^^^^

A recent Erlang should be installed from a custom PPA by running::

  sudo add-apt-repository ppa:scattino/ppa
  sudo apt-get update
  sudo apt-get install erlang-base erlang-ssl postgresql imagemagick


Debian (lenny)
^^^^^^^^^^^^^^

You'll need to build erlang from source. Before building, make sure
you install these packages::

  sudo apt-get install build-essential libncurses5-dev m4
  sudo apt-get install openssl libssl-dev
  sudo apt-get install unixodbc-dev

Alternatively, you can install a precompiled recent Erlang from the
following PPA: https://launchpad.net/~scattino/+archive/ppa

FreeBSD
^^^^^^^

If you're running on FreeBSD, make sure you've got the 'GNU' 'make'
(check with 'make --version', which should give you GNU, and version
info) If you're not running GNU-make as a default, edit the Makefile
to run with 'gmake' (make sure gmake is available first).


Mac OS X
^^^^^^^^

With MacPorts you can install Erlang and ImageMagick using the
following commands::

  sudo port install erlang +ssl
  sudo port install ImageMagick

EnterpriseDB has an excellent PostgreSQL installer available at
http://www.enterprisedb.com/products/pgdownload.do#osx


Windows
^^^^^^^

Currently, Zotonic is not officially supported on the Windows
platform. However, the main dependencies Erlang, PostgreSQL and
ImageMagick do work on Windows, so, if you're adventurous, it should
be possible to get it running.

We have included user-contributed ``start.cmd`` and ``build.cmd``
batch-scripts which are supposed to work on Windows.
