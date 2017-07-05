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
        bin/
        doc/
        docker/
        priv/
            log/
            sites/
            skel/
            ssl/
        user/
            models/
            sites/


``zotonic/``

    The main directory contains the startup script "start.sh" and the
    makefile "Makefile" to build the system.

``zotonic/_build/``

    The Rebar3 build directory.

``zotonic/doc/``

    Documentation sources files.

``zotonic/apps/zotonic_core/include/``

    The main zotonic include files.

``zotonic/apps/zotonic_mod_.../``

    All core modules of Zotonic. See the modules documentation for more
    information.

``zotonic/priv/``

    The priv directory is the place where all non core files are placed.

``zotonic/priv/log/``

    Here all the http access logs are written.

``zotonic/priv/ssl/``

    Holds the :ref:`SSL certificates <https-support>`.

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

``zotonic/_checkouts``

    This directory contains user-modifiable source code which runs in
    Zotonic, namely user-defined sites, modules and other Erlang/OTP
    applications.

``zotonic/_checkouts`` - sites

    A single Zotonic installation is capable of virtually hosting
    serving multiple sites. This directory holds the sites which are
    created and maintained by you, the users of Zotonic.

    This directory is the default location of the ``user_sites_dir``
    configuration variable. See :ref:`guide-configuration`.

``zotonic/_checkouts`` - modules

    This directory holds modules which are not part of the core Zotonic
    modules, but which are also not site-specific. All modules installed
    with the ``zotonic module install ...`` command are placed here.

    This directory is the default location of the ``user_modules_dir``
    configuration variable. See :ref:`guide-configuration`.
