Troubleshooting
===============

.. _ref-troubleshooting-installation:

Installation
------------

Site doesn’t start
^^^^^^^^^^^^^^^^^^

Check your database connection configuration in the ``config`` file which is
located in the :term:`user sites directory`. By default this is
``zotonic/user/sites/yoursite/config``.

Browsers can’t connect to http://yoursite:8000
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check the ``/etc/hosts`` file and make sure you have an entry like the following::

    127.0.0.1   yoursite

Zotonic won’t start and shows errors when running zotonic debug
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check your site’s database configuration.

Check PostgreSQL Authentication Config (``pg_hba.conf``).

If you get connection failures when starting Zotonic you should
double-check pg_hba.conf and make sure to ``/etc/init.d/postgresql
reload`` to make sure it gets loaded.

.. _ref-troubleshooting-sites:

Sites
-----

Unable to read boot script
^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases the Zotonic nodename does not resolve well and you see an error like::

    [error] Unable to read boot script (start_sasl.script): {error,enoent}

or::

    erl_call: can't ei_gethostbyname(MacBook-Pro-Apple)
    Zotonic is not running. You need to start Zotonic first to use this command.

Solution: Add the computer name to ``/etc/hosts``, for instance::

    127.0.0.1 MacBook-Pro-Apple

