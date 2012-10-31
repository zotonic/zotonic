.. _tutorial-zotonic_status:

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

The password that is used for this is a generated password which sites
in the ``priv/config`` file of the Zotonic folder.

.. image:: /img/zotonic_status_sites.png

The "update" buttons only appear when the site (or Zotonic itself) is
under Mercurial or Git revision control. These buttons do a "pull"
from the repository and then rebuild the system.
