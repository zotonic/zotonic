.. highlight:: bash

.. _guide-installation:

Installation
============

To get Zotonic running, you first need to install it. This chapter describes how
to install Zotonic manually. Alternatively, you can run Zotonic from one of
the :ref:`Docker images <guide-docker>`.

Preparation
-----------

First prepare your system for running Zotonic. Zotonic needs:

* Erlang 18 or higher
* PostgreSQL 8.4 or higher
* ImageMagick 6.5 or higher for image resizing
* Git for pulling in external dependencies

.. seealso::
    a more extensive discussion of
    :ref:`all requirements <installation-preinstall>`

On Linux
^^^^^^^^

For instance on Debian you can install the dependencies by running::

    $ sudo apt-get install build-essential git erlang imagemagick postgresql

On OS X
^^^^^^^

Install Homebrew_, then run::

    $ brew install erlang git imagemagick postgresql

.. _Homebrew: http://brew.sh

On Windows
^^^^^^^^^^

Currently, Zotonic is not officially supported on the Windows
platform. However, the main dependencies Erlang, PostgreSQL and
ImageMagick do work on Windows, so, if you’re adventurous, it should
be possible to get it running.

We have included user-contributed ``start.cmd`` and ``build.cmd``
batch-scripts which used to work on Windows, but have not been kept
up-to-date with recent changes. Expect some major tweaking to get this
back on track.

Getting Zotonic
---------------

1. Download the latest Zotonic release ZIP file from the `GitHub releases page`_. For
   instance:

   .. parsed-literal::
    $ wget \https://github.com/zotonic/zotonic/archive/|release|.zip

   Then unzip the file and rename the directory:

   .. parsed-literal::
    $ unzip |release|.zip
    $ mv zotonic-|release| zotonic

   Alternatively, clone the latest development version using Git::

    $ git clone https://github.com/zotonic/zotonic.git

2. You then need to compile the Zotonic sources::

    $ cd zotonic
    $ make

3. Then start Zotonic in debug mode::

    $ bin/zotonic debug

4. Now point your browser to: http://localhost:8000/.  You should see
   a welcome message, ‘Powered by Zotonic’. This is the so-called
   :ref:`status website <ref-status-site>`. So far, so good! Now it's
   time to :ref:`create your first site <guide-create-site>`.

Next steps
----------

* :ref:`Create your first site <guide-create-site>`.
* Log in to the :ref:`status site <ref-status-site>`.
* If something goes wrong, read the
  :ref:`troubleshooting reference <ref-troubleshooting-installation>`.
* Read more about Zotonic :ref:`configuration <guide-configuration>`.

.. _GitHub releases page: https://github.com/zotonic/zotonic/releases
