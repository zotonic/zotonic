.. _rel-0.44.0:

Release 0.44.0
==============

Welcome to Zotonic 0.44.0, TODO.

Main changes are:

* TODO

BC breaks
---------

* The password reset dispatch rule was changed. If you override ``email_password_reset.tpl``,
  you now need to pass the username too.

  Before::

    {% url logon_reset secret=secret use_absolute_url %}

  After::

    {% url logon_reset secret=secret u=username use_absolute_url %}

Commits since 0.43.0
--------------------

TODO
