.. _rel-0.50.0:

Release 0.50.0
==============

Welcome to Zotonic 0.50.0, released on June 22, 2019.

Main changes are:

  * Fix a problem with password resets on expired passwords
  * Fix a problem with password resets when the new password matches the old one

The ``exometer`` application was wrongly initialized since it was split into ``exometere_core``
and other applications. The default erlang configuraton had an entry for ``exometer``, you
should rename this in your ``erlang.config`` file to ``exometer_core``.


Commits since 0.49.2
--------------------

