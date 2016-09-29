Directory structure
===================

Zotonic’s directory structure is somewhat different from a regular
Erlang application::

    zotonic/
        _build
        _checkouts
        bin/
        doc/
        docker/
        ebin/
        include/
        modules/
        priv/
            log/
            sites/
            skel/
        src/
            behaviours/
            db/
            filewacher/
            i18n/
            install/
            markdown/
            models/
            scripts/
            smtp/
            support/
            tests/
        user/
            models/
            sites/


``zotonic/``

    The main directory contains the startup script "start.sh" and the
    makefile "Makefile" to build the system.

``zotonic/_build/``

    Zotonic’s dependencies as defined in ``rebar.config``.

``zotonic/doc/``

    Documentation sources files.

``zotonic/ebin/``

    All compiled erlang beam files are placed here.

``zotonic/include/``

    The main zotonic include files.

``zotonic/modules/``

    All core modules of Zotonic. See the modules documentation for more
    information.

``zotonic/priv/``

    The priv directory is the place where all non core files are placed.

``zotonic/priv/log/``

    Here all the http access logs are written.

``zotonic/priv/sites/``

    The directory containing sites which are internal to Zotonic. These
    are the ``zotonic_status`` site (see :ref:`ref-status-site`), and the
    ``testsandbox`` and ``testsandboxdb`` sites for running the unit tests.

``zotonic/src/``

    The Erlang source for the core Zotonic system.

``zotonic/src/behaviours/``

    Currently there are two behaviours defined. One for scomps and one
    for models.

``zotonic/src/db/``

    The database drivers. Currently, only PostgreSQL is supported as
    DBMS. As the modules and models sometimes use database specific
    queries (especially for full text search) it will not be easy to
    substitute an alternative database. Read this article why Zotonic
    only supports PostgreSQL.

``zotonic/src/i18n/``

    Internationalization support.

``zotonic/src/install/``

    The database model and basic installation for sites. The files here
    are used when an empty database is found and needs to be installed.

``zotonic/src/models/``

    The data model access files. Implements internal APIs for the
    different data models for use in Erlang modules and
    templates. Examples of datamodels are m_rsc, m_config and
    m_category.

``zotonic/src/support/``

    All base Zotonic source code. Here you will find the source code for
    site supervisors, module supervisors, image resize server, context
    routines, and much more.

``zotonic/src/tests/``

    Contains the EUnit tests for Zotonic.

``zotonic/user``

    This directory contains user-modifiable source code which runs in
    Zotonic, namely user-defined sites and modules.

``zotonic/user/sites/``

    A single Zotonic installation is capable of virtually hosting
    serving multiple sites. This directory holds the sites which are
    created and maintained by you, the users of Zotonic.

    This directory is the default location of the ``user_sites_dir``
    configuration variable. See :ref:`guide-configuration`.

``zotonic/user/modules/``

    This directory holds modules which are not part of the core Zotonic
    modules, but which are also not site-specific. All modules installed
    with the ``zotonic module install ...`` command are placed here.

    This directory is the default location of the ``user_modules_dir``
    configuration variable. See :ref:`guide-configuration`.
