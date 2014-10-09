.. _installation-zotonic_status:

The Zotonic "status" site
=========================

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

When logged in to the Zotonic status site, you can manage the running
sites on the system: starting, stopping and upgrading them.

Upon first visit, the site shows a friendly message, which tells
visitors that the site they are looking at has probably not been
configured correctly yes. It also asks for a password to log in.

The password for this site is automatically generated and stored in ``~/.zotonic/zotonic.config``.

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
