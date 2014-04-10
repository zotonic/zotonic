Unit testing
============

Zotonic comes with a basic test suite which can be run the following way::

  zotonic runtests

This starts the Zotonic system and executes all EUnit tests. It will
disable all zotonic sites except for the special site 'testsandbox',
which will be enabled.

The `testsandbox` site does not have a database configuration and is
configured to run on `localhost:8040`.
  
