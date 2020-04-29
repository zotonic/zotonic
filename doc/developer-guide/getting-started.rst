.. highlight:: bash

Getting Started
===============

You have two options for running Zotonic: to get started quickly, start our
Zotonic container. Or you can install Zotonic on your computer.

Docker
------

First `download and install Docker`_ . Then build and start Zotonic with a single
command::

    $ ./start-docker.sh

Docker will download and boot the container. The container will start building
Zotonic. After which Zotonic can be started with::

    bash-4.4$ bin/zotonic debug

Zotonic will be available on port 8443 on your machine. Open your browser and go to
https://localhost:8443/ to view the Zotonic Status page. If you wish to quit
Zotonic, press twice Ctrl+C. If you wish to quite the container, press Ctrl+D at the
bash prompt.

To see the configuration for the password (and other configurations) for the zotonic
status site, type::

    bash-4.4$ bin/zotonic config

The password is the *password* entry, the username is always ``wwwadmin``.

You can now move on to :ref:`creating your first site <guide-create-site>`.

.. seealso::

    :ref:`guide-docker` for more information on using Docker and how to find the
    password for the Zotonic status site.

.. _guide-installation:

Cloud-Init
----------

A cloud-init file is supplied in `zotonic_launcher <https://github.com/zotonic/zotonic/blob/master/cloud-init/zotonic-cloudinit.yml>`_.

This file can be used to install a VPS by providers that support cloud-init. Hetzner is one such provider.

After the cloud-init is done with its installation a new server is up and running on port 80 and 443.
It will be using a self-signed certificate, located in ``/home/zotonic/.zotonic/security/self-signed/``.

The ``wwwadmin`` password for the zotonic status site can be found after logging in to your server::

    root:~# sudo su - zotonic
    zotonic:~$ cd zotonic
    zotonic:~/zotonic$ bin/zotonic config

    Zotonic config for 'zotonic@yourvps':
    =================================

    zotonic:
        environment: production
        zotonic_apps: /home/zotonic/zotonic/apps_user
        security_dir: /home/zotonic/.zotonic/security
        password: wXuqsZkC4qp8j1AZHyO3
        timezone: UTC
        ...

The server can be stopped and started using the command line::

    zotonic:~/zotonic$ bin/zotonic stop
    Stopping zotonic 'zotonic@yourvps' .... OK
    zotonic:~/zotonic$ bin/zotonic start
    Waiting for zotonic: . OK



Installation
------------

If you don’t like Docker, or you like to do things yourself, you can always
install Zotonic on your computer yourself.

Preparation
^^^^^^^^^^^

First prepare your system for running Zotonic. Zotonic needs:

* Erlang 19 or higher
* PostgreSQL 8.4 or higher
* ImageMagick 6.5 or higher for image resizing
* Git for pulling in external dependencies
* C++ compiler (gcc) for erl_exec and other dependencies

.. seealso::
    a more extensive discussion of
    :ref:`all requirements <installation-preinstall>`


Ubuntu / Debian
^^^^^^^^^^^^^^^

We recommend you install Erlang from the Erlang solutions website:

https://www.erlang-solutions.com/downloads/download-erlang-otp

The other requirements are easily fetched with ``apt``::

  sudo sudo apt-get install gcc g++ build-essential git imagemagick postgresql

macOS
"""""

Install Homebrew_, then run::

    $ brew install erlang git imagemagick postgresql

.. _Homebrew: https://brew.sh

FreeBSD
"""""""

Erlang and its dependencies can be installed with ``pkg``::

  # pkg install sudo zip wget bash gmake curl git gcc erlang

Also install ImageMagick and PostgreSQL, at the time of writing the commands below
can be used, they should be updated with the newest available version::

  # pkg install ImageMagick7-nox11
  # pkg install postgresql10-server


Windows
"""""""

Currently, Zotonic is not officially supported on the Windows
platform. However, the main dependencies Erlang, PostgreSQL and
ImageMagick do work on Windows, so, if you’re adventurous, it should
be possible to get it running.

It is advised to use Docker or the Linux subsystem for Windows.


Getting Zotonic
^^^^^^^^^^^^^^^

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

4. Now point your browser to: https://localhost:8443/.  You should see
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

.. _download and install Docker: https://www.docker.com/products/docker
.. _GitHub releases page: https://github.com/zotonic/zotonic/releases
