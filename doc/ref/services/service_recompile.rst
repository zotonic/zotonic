
.. include:: meta-recompile.rst

Remotely recompile and flush.

This GET requests performs a ``z:m()`` call which recompiles all of
Zotonic’s Erlang modules and all Erlang modules in all Zotonic sites.

When done recompiling, it calls ``z:flush()`` to flush the memo
caches.

This call is mostly used for development purposes; for production use,
:ref:`mod_development` should not be enabled.
             
