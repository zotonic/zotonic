.. highlight:: bash

.. _guide-docker:

Docker
======

We offer two Docker images:

* `zotonic/zotonic-full`_ contains both Zotonic and PostgreSQL. Use this to get
  started quickly.
* `zotonic/zotonic`_ contains only Zotonic. This image is most useful in
  production setups or when you’re using Docker Compose with a separate
  database container.

Start a Zotonic image on your Docker machine::

    # use a stable version:
    docker run -d -p 8000:8000 zotonic/zotonic-full:0.17.0

    # or a branch:
    docker run -d -p 8000:8000 zotonic/zotonic-full:0.x

    # or run the latest version from master:
    docker run -d -p 8000:8000 zotonic/zotonic-full:latest

Mount a volume that contains your Zotonic sites::

    docker run -d -v `pwd`/sites:/opt/zotonic/user/sites zotonic/zotonic-full

And mount a volume with your custom Zotonic modules::

    docker run -d -v `pwd`/sites:/opt/zotonic/user/sites -v `pwd`/modules:/opt/zotonic/user/modules zotonic/zotonic-full

.. _zotonic/zotonic-full: https://hub.docker.com/r/zotonic/zotonic-full/
.. _zotonic/zotonic: https://hub.docker.com/r/zotonic/zotonic/
