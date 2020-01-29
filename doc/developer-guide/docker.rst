.. highlight:: bash

.. _guide-docker:

Docker
======

We offer the Docker image `zotonic/zotonic-dev`_ which contains build tools and Erlang.

It can be used for development work on Zotonic itself, where you’ll mount your Zotonic directory
as a volume in the container.

To use this image, first `download and install Docker`_.


zotonic-dev
-----------

You can use the `zotonic/zotonic-dev`_ image for doing development work
on Zotonic and Zotonic sites.

Start the container from your local Zotonic clone::

    $ git clone https://github.com/zotonic/zotonic.git

Start the container::

    $ cd zotonic
    $ ./start-docker.sh

This uses Docker Compose to start the Zotonic container and a PostgreSQL container.

On first start Zotonic will be compiled and all dependencies are fetched.
This can take quite some time if you are not running on Linux (Docker’s file i/o is
quite inefficient on non-Linux platforms).

A shell prompt will appear after the build is complete. Now Zotonic can be
started::

    bash-4.4$ bin/zotonic debug

You can stop Zotonic (Erlang) by typing Ctrl+C twice.

The configuration, including the Zotonic status site password, can be found with::

    bash-4.4$ bin/zotonic config

Zotonic’s port 8443 and 8000 are exposed as local ports. You can  view the
:ref:`Zotonic status page <ref-status-site>` at ``https://localhost:8443/``.
You can log in using the username `wwwadmin` and the password from the config.

Zotonic is running with a self-signed certificate. The certificate can be found
in ``docker-data/etc-zotonic/security/self-signed/``.

You can also run other commands in the container, such as running the tests::

    $ bin/zotonic runtests

Any changes you make in the Zotonic source files will be propagated to the
container and :ref:`automatically compiled <automatic-recompilation>`.

You can stop the container using Ctrl+D at the Bash shell prompt.


.. _zotonic/zotonic-dev: https://hub.docker.com/r/zotonic/zotonic-dev/
.. _Docker Compose: https://docs.docker.com/compose/
.. _download and install Docker: https://www.docker.com/products/docker
