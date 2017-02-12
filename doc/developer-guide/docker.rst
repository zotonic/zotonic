.. highlight:: bash

.. _guide-docker:

Docker
======

We offer three Docker images:

* `zotonic/zotonic-full`_ contains both Zotonic and PostgreSQL. Use this to get
  started quickly if you want to build sites on Zotonic.
* `zotonic/zotonic`_ contains only Zotonic. This image is most useful in
  production setups or when you’re using `Docker Compose`_ with a separate
  database container.
* `zotonic/zotonic-dev`_ contains build tools and Erlang. Use this image for
  development work on Zotonic itself, where you’ll mount your Zotonic directory
  as a volume in the container.

To use any of the images, first `download and install Docker`_.

Start a Zotonic image on your Docker machine::

    # use a stable version:
    $ docker run -d -p 8000:8000 zotonic/zotonic-full:0.17.0

    # or a branch:
    $ docker run -d -p 8000:8000 zotonic/zotonic-full:0.x

    # or run the latest version from master:
    $ docker run -d -p 8000:8000 zotonic/zotonic-full:latest

Mount a volume that contains your Zotonic sites::

    $ docker run -d -v `pwd`/sites:/opt/zotonic/user/sites zotonic/zotonic-full

And mount a volume with your custom Zotonic modules::

    $ docker run -d -v `pwd`/sites:/opt/zotonic/user/sites -v `pwd`/modules:/opt/zotonic/user/modules zotonic/zotonic-full

zotonic-dev
-----------

You can use the `zotonic/zotonic-dev`_ image when you’re doing development work
on Zotonic. Start the container from your local Zotonic clone::

    $ git clone https://github.com/zotonic/zotonic.git

To start the container, use Docker Compose::

    $ docker-compose run --service-ports zotonic sh

This opens a shell prompt in the Zotonic container. A PostgreSQL container will
be started automatically as well. In the Zotonic container, you can enter any
command. So, to start Zotonic in debug mode::

    $ bin/zotonic debug

The ``--service-ports`` flags exposes Zotonic’s port 8000 as your local port 80,
so you can view the :ref:`Zotonic status page <ref-status-site>` at
``http://localhost``.

You can also run other commands in the container, such as running the tests::

    $ bin/zotonic runtests

Any changes you make in the Zotonic source files will be propagated to the
container and :ref:`automatically compiled <automatic-recompilation>`.

.. _zotonic/zotonic-full: https://hub.docker.com/r/zotonic/zotonic-full/
.. _zotonic/zotonic: https://hub.docker.com/r/zotonic/zotonic/
.. _zotonic/zotonic-dev: https://hub.docker.com/r/zotonic/zotonic-dev/
.. _Docker Compose: https://docs.docker.com/compose/
.. _download and install Docker: https://www.docker.com/products/docker
