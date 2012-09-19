Installation
============

You will need to:

1. Install Erlang R14B03 or newer. Build it from source, or
   use packages (see platform-specific notes below)

2. Install ImageMagick (version 6.5 or higher) for the 'convert' and
   'identify' tools.  Make sure that the convert and identify tools
   are in your path so that zotonic can find them. For auto-rotation
   to work you'll need the "exif" utility as well.

3. Install PostgreSQL (preferably 8.3 or newer).

4. Enable trust-authentication (username+password) in postgres.

5. Obtain a copy of the Zotonic source code.



Steps to install Zotonic
------------------------

1. Type "make" in the root of zotonic (there where the Makefile is located). (*1)

2. Create an user and database in PostgreSQL (change the password for the user!):

   CREATE USER zotonic WITH PASSWORD 'zotonic';
   CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
   GRANT ALL ON DATABASE zotonic TO zotonic;
   \c zotonic
   CREATE LANGUAGE "plpgsql";

3. Create a new zotonic site, based on the "blog" skeleton site:

   bin/zotonic addsite -s blog yoursite

   This will add a site named yoursite. Its default URL will be
   http://yoursite:8000/ so either put 'yoursite' in your hosts
   file or change the {hostname} section of the config file.

4. Edit the generated file priv/sites/yoursite/config, to make sure
   your database credentials and the hostname are correct, and change
   the password for the admin.

5. Start zotonic in debug mode:

   bin/zotonic debug

7. You see zotonic starting up, lots of messages pass by, and zotonic
   will install the initial database.  When something goes wrong here,
   then it is almost always a problem with the database
   connection. Check your database configuration in the zotonic.sh
   file.

8. Point your browser to 
	
    http://yoursite:8000/
	
   or logon as user 'admin' (the default password is 'admin') at:

    http://yoursite:8000/admin/

9. When all done, then you can stop the erlang shell with:

    q().

   or pressing ctrl-c twice.



Operating system specific notes
-------------------------------


Ubuntu ( >= 10.04)
^^^^^^^^^^^^^^^^^^

Erlang can be installed from a custom PPA by running:

sudo add-apt-repository ppa:scattino/ppa
sudo apt-get update
sudo apt-get install erlang-base postgresql imagemagick


Ubuntu 9.04 (jaunty)
^^^^^^^^^^^^^^^^^^^^

You'll need to build erlang from source. Before building, make sure
you install these packages:

sudo apt-get install build-essential unixodbc-dev libncurses-dev libssl-dev libxml2-dev libexpat1-dev

PostgreSQL and Imagemagick are available on Ubuntu as packages:

sudo apt-get install postgresql-8.4 imagemagick


Debian (lenny)
^^^^^^^^^^^^^^

You'll need to build erlang from source. Before building, make sure
you install these packages:

sudo apt-get install build-essential libncurses5-dev m4
sudo apt-get install openssl libssl-dev
sudo apt-get install unixodbc-dev

Alternatively, you can install a precompiled recent Erlang from the
following PPA: https://launchpad.net/~scattino/+archive/ppa

Afterwards, for a quick setup of zotonic and postgres you can use the
following script:
https://raw.github.com/zotonic/zotonic/master/zotonic_install


FreeBSD
^^^^^^^

If you're running on FreeBSD, make sure you've got the 'GNU' 'make'
(check with 'make --version', which should give you GNU, and version
info) If you're not running GNU-make as a default, edit the Makefile
to run with 'gmake' (make sure gmake is available first).


Windows
^^^^^^^

Currently, Zotonic is not officially supported on the Windows
platform. However, the main dependencies Erlang, PostgreSQL and
ImageMagick do work on Windows, so, if you're adventurous, it should
be possible to get it running.

We have included user-contributed "start.cmd" and "build.cmd"
batch-scripts which are supposed to work on Windows.


Mac OS X
^^^^^^^^

With MacPorts you can install Erlang and ImageMagick using the
following commands:

  sudo port install erlang +ssl
  sudo port install ImageMagick

EnterpriseDB has an excellent PostgreSQL installer available at
http://www.enterprisedb.com/products/pgdownload.do#osx

For a very basic step-by-step installation on OSX, chick this
http://timbenniks.nl/blog/712/step-by-step-guide-to-install-zotonic-on-osx.
