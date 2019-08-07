.. _ref-status-site:

The Status site
===============

The Zotonic "status" site is the first thing you see once you have
installed Zotonic, when you do not have any sites configured yet. This
is what it looks like:

.. image:: /img/zotonic_status_login.png

This site is also the `fallback` site for Zotonic.

Since Zotonic supports virtual hosting, it uses the HTTP ``Host:``
parameter to see which site should be served at which URL. If it does
not find a ``Host:`` header, or if the host header does not correspond
to any known Zotonic site, it shows the ``zotonic_status`` site
instead.


Logging in
----------

Upon first visit, the site shows a friendly message, which tells
visitors that the site they are looking at has probably not been
configured correctly yet.

Pressing the ``Manage this server`` button will bring up the login
dialog.

The username for the status site is ``wwwadmin``. The password is
automatically generated and stored in ``~/.zotonic/[release]/zotonic.config``,
where ``[release]`` is the Zotonic release number, like ``1.0``.

When logged in to the Zotonic status site, you can manage the running
sites on the system: starting, stopping and upgrading them.

.. image:: /img/zotonic_status_sites.png

The "update" buttons only appear when the site (or Zotonic itself) is
under Mercurial or Git revision control. These buttons do a "pull"
from the repository and then rebuild the system.


Getting the global sites status
-------------------------------

The Zotonic status sites exposes a small API service which allows you
to check whether all of your sites are still running::

  http://yourzotonichost.com/api/zotonic_status/check

It returns a JSON response of ``{"status":"ok"}`` when every Zotonic
site is running.

``"Running"`` means that a site’s status is not ``"retrying"`` or ``"failed"``; so
it does not count sites that you have manually stopped from the
interface.

This API service can be plugged in to a service like
https://www.pingdom.com/ to monitor the availability of all hosted sites
at once.

.. _restart-site:

Restarting sites
----------------

Alternatively, from the :ref:`Zotonic shell <guide-cli-shell>`::

    z_sites_manager:restart(yoursitename).
