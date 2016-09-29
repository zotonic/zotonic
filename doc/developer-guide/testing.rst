.. _dev-testing:

Testing sites
=============

It is possible to create end-to-end integration tests for Zotonic
websites.  Tests like these are called *sitetests*. They run within
the Zotonic shell, starting the website under test using a special
database schema so that the main database is not affected by the
tests.

To run the sitetests for a site called ``example``, run the following command::

  z_sitetest:run(example).

This will stop the ``example`` site, create a new database schema
(called ``z_sitetest``), start the site (which installs all the site's data, e.g. those defined by the :ref:`guide-modules-versioning`,
into the new schema), and then scans all compiled Erlang modules for
modules named ``example_*_sitetest.erl``. These found test modules are
then run using Erlang's standard `EUnit test framework <http://erlang.org/doc/apps/eunit/chapter.html>`_.

.. note:: The filename pattern of the tests is
   ``(sitename)_(testname)_sitetest.erl``, where ``sitename`` is the name
   of the site under test (lowercase alphanumerics + underscores), and
   ``testname`` is a name for the test suite (lowercase alphanumerics
   only). ``testname`` *cannot* contain any underscores.


Besides running the tests from the Erlang shell, they can also be run
from the terminal :ref:`ref-cli`:

.. code-block:: bash

    $ zotonic sitetest example

This is convenient for integration into CI systems.


Example ``sitetest`` module
---------------------------

Put the following inside the ``example`` site under the filename
``tests/example_administrator_sitetest.erl``:

.. code-block:: erlang

     -module(example_administrator_sitetest).
     -compile(export_all).

     -include_lib("eunit/include/eunit.hrl").

     sudo_context() ->
       z_acl:sudo(z:c(example)).

     administrator_name_test() ->
       ?assertEqual(<<"Site Administrator">>, m_rsc:p(1, title, sudo_context())),
       ok.


Test Driven Development
-----------------------

Running the sitetests is integrated into the filewatcher hooks. As
soon as you edit a ``*_sitetest.erl`` file, all tests in that specific
sitetest file will be executed.

While developing a site it is often handy to continuously run the
tests while you are developing the site.

To establish this, you can call ``z_sitetest:watch(site)`` to start
watching all Erlang files inside the site under development. When the
filetest is watching a site, all filetests are triggered as soon as
any Erlang module inside your site is being recompiled.

To stop the automatic test running again, call
``z_sitetest:unwatch(site)``.



Example testing output
----------------------

Running the test command ``z_sitetest:run(example).``, will produce output similar to the following::

    (zotonic001@host)33> z_sitetest:run(example).
    15:45:18.162 [info] Site stopped: example (<0.17763.0>)
    15:45:18.682 [warning] [example] Database connection failure: noschema
    15:45:18.688 [warning] [example] Creating schema "z_sitetest" in database "example"
    15:45:18.693 [info] [example] Retrying install check after db creation.
    15:45:18.702 [warning] [example] Installing database with db options: [{dbschema,"z_sitetest"},{dbdatabase,"example"},{dbhost,"localhost"},{dbport,5432},{dbuser,"zotonic"}]
    15:45:19.226 [info] example: Install start.
    15:45:19.226 [info] Inserting categories
    15:45:19.257 [info] Inserting base resources (admin, etc.)
    15:45:19.264 [info] Inserting username for the admin
    15:45:19.267 [info] Inserting predicates
    15:45:19.290 [info] example: Install done.
    15:45:19.299 [info] Site started: example (<0.22082.0>)
    15:45:19.575 [info] [example] info @ z_datamodel:169  Creating new category 'menu'
    15:45:19.604 [info] [example] info @ z_datamodel:169  Creating new menu 'main_menu'
    15:45:19.723 [info] [example] info @ z_datamodel:169  Creating new category 'content_group'
    15:45:19.755 [info] [example] info @ z_datamodel:169  Creating new content_group 'system_content_group'
    15:45:19.775 [info] [example] info @ z_datamodel:169  Creating new content_group 'default_content_group'
    15:45:19.893 [info] [example] info @ z_datamodel:169  Creating new category 'acl_user_group'
    15:45:19.959 [info] [example] info @ z_datamodel:169  Creating new category 'acl_collaboration_group'
    15:45:20.001 [info] [example] info @ z_datamodel:169  Creating new predicate 'hasusergroup'
    15:45:20.033 [info] [example] info @ z_datamodel:169  Creating new predicate 'hascollabmember'
    15:45:20.044 [info] [example] info @ z_datamodel:169  Creating new predicate 'hascollabmanager'
    15:45:20.059 [info] [example] info @ z_datamodel:169  Creating new acl_user_group 'acl_user_group_anonymous'
    15:45:20.071 [info] [example] info @ z_datamodel:169  Creating new acl_user_group 'acl_user_group_members'
    15:45:20.079 [info] [example] info @ z_datamodel:169  Creating new acl_user_group 'acl_user_group_editors'
    15:45:20.086 [info] [example] info @ z_datamodel:169  Creating new acl_user_group 'acl_user_group_managers'
    15:45:20.393 [info] [example] info @ z_datamodel:169  Creating new category 'admin_content_query'
    ======================== EUnit ========================
    example_testone_sitetest: administrator_name_test (module 'example_administrator_sitetest')...[0.022 s] ok
    =======================================================
      Test passed.
    ok
