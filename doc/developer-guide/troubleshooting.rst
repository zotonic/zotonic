Troubleshooting
===============

.. _ref-troubleshooting-installation:

Installation
------------

Zotonic won’t start and shows errors when running zotonic debug
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check your site’s database configuration.

Check PostgreSQL Authentication Config (``pg_hba.conf`` in ``/etc/postgresql``).

If you get connection failures when starting Zotonic you should
double-check ``pg_hba.conf`` and make sure to ``/etc/init.d/postgresql
reload`` to make sure it gets loaded.

The ``pg_hba.conf`` should contain something like:

.. code-block:: none

    # IPv4 local connections:
    host    all             all             127.0.0.1/32            md5
    # IPv6 local connections:
    host    all             all             ::1/128                 md5


Unable to read boot script
^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases the Zotonic nodename does not resolve well and you see an error like:

.. code-block:: none

    [error] Unable to read boot script (start_sasl.script): {error,enoent}

or:

.. code-block:: none

    erl_call: can't ei_gethostbyname(MacBook-Pro-Apple)
    Zotonic is not running. You need to start Zotonic first to use this command.

Solution: Add the computer name to ``/etc/hosts``, for instance:

.. code-block:: none

    127.0.0.1 MacBook-Pro-Apple

Erlang crashes
--------------

Here we list some common causes of Erlang crashes.

``{{badmatch,{error,emfile}}`` or ``{reason, emfile}``

You need to raise the :ref:`file-descriptors` limit.


.. _ref-troubleshooting-sites:

Sites
-----

Site doesn’t start
^^^^^^^^^^^^^^^^^^

Check your database connection configuration in the ``zotonic_site.config``
file which is located in the :term:`zotonic user directory`. This can be found
in ``yoursite/priv/zotonic_site.config``. The priv directory should also be
soft linked in the ``_build`` directory: ``_build/default/lib/yoursite/priv/zotonic_site.config``

Browsers can’t connect to http://yoursite.test:8000
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check the ``/etc/hosts`` file and make sure you have an entry like the following:

.. code-block:: none

    127.0.0.1   yoursite.test

Browsers give self-signed certificate warning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you connect to ``https://yoursite.test:8443`` the browser warns that the
site is not trusted. This is correct, as the certificate is self-signed.

You can connect if you accept the certificate. For Safari and Firefox just follow
the instructions shown to you by the browser.

For Chrome you have to do something silly:

 * Click anywhere in the white on the window
 * Type: ``thisisunsafe``

You won‘t see any characters echo to the screen. After typing the magic
strings the page should reload and the site certificate is accepted.
