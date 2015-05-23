.. highlight:: sh
.. _installation-install:
  
Installing Zotonic
==================

   
Before beginning installation, please be sure your system meets the
:ref:`install-requirements`.

At the end of this page, you should have a bare-bones Zotonic running,
without any sites in it.

There are multiple ways of installing Zotonic, depending on what you want and 

1. :ref:`installation-install-release`
2. :ref:`installation-install-git`
3. :ref:`installation-install-deb`
4. :ref:`installation-install-script`

For most people, :ref:`installation-install-release` should be the best option.
   

.. _installation-install-release:
   
Installing from a release .zip archive
--------------------------------------

Zotonic versions are released as `.zip` files from the official
download page. This installation method gets you the officially
supported version.

1. Download the latest release as a `.zip` from
   https://github.com/zotonic/zotonic/releases, and unzip it in a
   folder called ``zotonic``.

.. include:: _make_common.rst

.. _installation-install-git:

Installing from `git`
----------------------------------------------

If you want to run the latest version of Zotonic or run on one of its
release branches, get a copy of the source tree from github.com and
run Zotonic from there.

1. Use `git <http://git-scm.com/>`_ to `clone` a copy of the Zotonic source tree::

     git clone git://github.com/zotonic/zotonic.git
     cd zotonic

   You are now in the master branch, which contains the current
   development version of Zotonic. To switch to the more stable 0.12
   version, switch to the ``release-0.12.x`` branch::

     git checkout release-0.12.x

.. include:: _make_common.rst

             
.. _installation-install-deb:

Installing from the Debian package
----------------------------------             

**At this time it is NOT recommended to use PPA / deb install method because
the PPA is not up to date with upstream releases. (23.05.2015)**

Official Zotonic releases are packaged as a ``.deb`` package. This
packages installs all prerequisites, creates a ``zotonic`` user, and
sets up a Zotonic instance running from
``/var/lib/zotonic/zotonic``. This package works for both Ubuntu and
Debian distributions, and is the preferred way of running Zotonic on
Debian for production purposes.

Ubuntu users can do the following to install this package::

  sudo add-apt-repository ppa:arjan-scherpenisse/zotonic

Debian users need to add the following line to their ``/etc/apt/sources.list`` file::

  deb http://ppa.launchpad.net/arjan-scherpenisse/zotonic/ubuntu lucid main 

Then, run::

  sudo apt-get update && sudo apt-get install zotonic
  
Now, point your browser to http://localhost:8000/ and make sure
you see the `Powered by Zotonic` welcome screen. Then, head on to
:ref:`installation-install-addsite`.

  
.. _installation-install-script:

Installation with the one-line script for Debian-based systems
--------------------------------------------------------------

.. note:: This script installs the latest development version, so be
          careful not to use it on a production system.

For a one-liner install of Zotonic and its dependencies you can use
the following script::

  wget -O - https://raw.github.com/zotonic/zotonic/master/zotonic_install | bash

This will install all dependencies, clone the Zotonic `git`
repository, create a ``zotonic`` user, add the necessary database
permissions, and compile and start Zotonic.

Now, point your browser to http://localhost:8000/ and make sure
you see the `Powered by Zotonic` welcome screen. Then, head on to
:ref:`installation-install-addsite`.
