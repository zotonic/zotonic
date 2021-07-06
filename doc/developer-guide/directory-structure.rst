Directory structure
===================

Zotonic is a set of regular OTP applications. These can be found in the
repository’s `apps/` directory::

    zotonic/
        _build
        _checkouts
        apps/
            zotonic_core/
            zotonic_mod_acl_user_groups/
            zotonic_mod...
            zotonic_site_status
            zotonic_site_testsandbox
            ...
        apps_user
            mysite
            ...
        bin/
        doc/
        docker/
        priv/
            log/
            sites/
            skel/
            ssl/

``zotonic/``

    The main directory contains the startup script "start.sh" and the
    makefile "Makefile" to build the system.

``zotonic/_build/``

    The Rebar3 build directory.

``zotonic/_checkouts``

    Place here checkouts of dependencies that are in development and
    should be used instead of the dependencies in ``rebar.lock`` or
    as mentioned in the ``rebar.config`` files of all other apps.

``zotonic/bin/``

    Shell scripts for the Zotonic :ref:`ref-cli` and scripts for maintaining
    the translation ``.po`` files.

``zotonic/doc/``

    Documentation sources files.

``zotonic/apps/zotonic_core/include/``

    The main zotonic include files.

``zotonic/apps/zotonic_mod_.../``

    All core modules of Zotonic. See the modules documentation for more
    information.

``zotonic/priv/``

    The priv directory is the place where all non core files statis assets are placed.

``zotonic/logs/``

    Here all the logs are written.

``zotonic/data/``

    Here all the data is written. These are uploaded files, mnesia files etc.

``zotonic/data/sites/<sitename>``

    Here all the data for a specific site is written. Primarily uploaded files and generated previews.

``zotonic/apps/zotonic_core/src/``

    The Erlang source for the core Zotonic system.

``zotonic/apps/zotonic_core/src/behaviours/``

    Currently there are two behaviours defined. One for scomps and one
    for models.

``zotonic/apps/zotonic_core/src/db/``

    The database drivers. Currently, only PostgreSQL is supported as
    DBMS. As the modules and models sometimes use database specific
    queries (especially for full text search) it will not be easy to
    substitute an alternative database. Read this article why Zotonic
    only supports PostgreSQL.

``zotonic/apps/zotonic_core/src/i18n/``

    Internationalization support.

``zotonic/apps/zotonic_core/src/install/``

    The database model and basic installation for sites. The files here
    are used when an empty database is found and needs to be installed.

``zotonic/apps/zotonic_core/src/models/``

    The data model access files. Implements internal APIs for the
    different data models for use in Erlang modules and
    templates. Examples of datamodels are m_rsc, m_config and
    m_category.

``zotonic/apps/zotonic_core/src/support/``

    All base Zotonic source code. Here you will find the source code for
    site supervisors, module supervisors, image resize server, context
    routines, and much more.

``zotonic/apps/zotonic_core/test/``

    Contains the EUnit tests for Zotonic.

``zotonic/apps/zotonic_site_status``

    The Zotonic :ref:`status site <ref-status-site>`.

``zotonic/apps/zotonic_site_testsandbox``

    The Zotonic testsandbox site that tests are run against.

``zotonic/apps_user``

    This directory contains user-modifiable source code which runs in
    Zotonic, namely user-defined sites, modules and other Erlang/OTP
    applications.

    The ``apps_user`` directory is the default location of the
    ``ZOTONIC_APPS`` environment variable. See :ref:`guide-deployment-env`.


