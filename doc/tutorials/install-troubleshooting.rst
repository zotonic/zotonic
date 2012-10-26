Installation troubleshooting
============================

Browsers can't connect to http://blog:8000/
-------------------------------------------

Check the ``/etc/hosts`` file and make sure you have an entry like the following::

  127.0.0.1     blog


Zotonic won't start and shows errors when running zotonic debug
----------------------------------------------------------------

Check PostgreSQL Authentication Config (``pg_hba.conf``).

If you get connection failures when starting Zotonic you should
double-check pg_hba.conf and make sure to ``/etc/init.d/postgresql
reload`` to make sure it gets loaded.
