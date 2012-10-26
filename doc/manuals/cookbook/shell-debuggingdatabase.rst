Debugging db (query) issues
===========================

Techniques for finding root cause when queries are involved.

Why
---

When you face unexpected behavior as a result of some database query
(z_db:q et al), you either have to hunt down the queries and re-run
them by hand, which is really time consuming and painful, or, which
this cookbook will detail, have postgresql log the queries for you.

The principal documentation and with more details, may be found here:
http://www.postgresql.org/docs/8.4/static/runtime-config-logging.html

Assumptions
-----------

A working system where you want to inspect the db queries being run.

How
---

Edit your ``postgresql.conf`` file, enabling ``logstatement =
'all'``. This file has a lot of options commented out, with comments
which you may review. Options of interest in this scenario all begin
with "log" (or similar).

By having ``log_destination = 'stderr'`` and ``logging_collector =
on``, you can capture your logging output to file.

