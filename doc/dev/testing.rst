Unit testing
============

Zotonic comes with a basic test suite which can be run the following way::

  zotonic runtests

This starts the Zotonic system and executes all EUnit tests. 

However, you will see a lot of failing tests, since the sandboxed test
environment is not started by default, as this needs some setup
regarding the database and hostname.

The sandbox test environment is located at ``priv/sites/testsandbox``, and
behaves just like a regular zotonic site. Create a database like this::

  CREATE DATABASE zotonic_testsandbox  WITH OWNER = zotonic ENCODING = 'UTF8';
  GRANT ALL ON database zotonic_testsandbox TO zotonic;

Rename the ``priv/sites/testsandbox/config.in`` file to config, and edit
it to adjust the database settings.

Then create a hosts entry pointing to localhost::

  127.0.0.1    testsandbox

After this change, the test suite should run successfully.
  
