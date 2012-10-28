Directory structure
===================

Zotonic's directory structure is somewhat different from a regular
Erlang application::

  zotonic/
    deps/
    doc/
    ebin/
    include/
    modules/
    priv/
        custom_tags/
        log/
        sites/
            default/
    src/
        behaviours/
        dbdrivers/
            postgresql/
        erlydtl/
        i18n/
        install/
        models/
        support/
        tests/

``zotonic/``

  The main directory contains the startup script "start.sh" and the
  makefile "Makefile" to build the system.

``zotonic/deps/``

  The OTP applications Zotonic depends on. Currently contains
  Webmachine, MochiWeb and erlang-oauth.

``zotonic/doc/``

  Some very simple documentation and installation guide. The main
  documentation is on this site. Also contains installation
  instructions for using Zotonic with nginx and an example Varnish
  configuration file.

``zotonic/ebin/``

  All compiled erlang beam files are placed here.

``zotonic/include/``

  The main zotonic include file "zotonic.hrl" and basic Webmachine
  resources used as a basis by other resource modules.

``zotonic/modules/``

  All core modules of Zotonic. See the modules documentation for more
  information.

``zotonic/priv/``

  The priv directory is the place where all non core files are placed.

``zotonic/priv/custom_tags/``

  Custom tags for the ErlyDTL templates.

``zotonic/priv/log/``

  Here all the http access logs are written, there is currently no
  split between different sites.

``zotonic/priv/sites/``

  The directory containing all sites. A single Zotonic system can
  serve multiple sites. See the documentation about sites for more
  information.

``zotonic/src/``

  The Erlang source for the core Zotonic system.

``zotonic/src/behaviours/``

  Currently there are two behaviours defined. One for scomps and one
  for models.

``zotonic/src/dbdrivers/``

  The database drivers. Currently, only PostgreSQL is supported as
  DBMS. As the modules and models sometimes use database specific
  queries (especially for full text search) it will not be easy to
  substitute an alternative database. Read this article why Zotonic
  only supports PostgreSQL.

``zotonic/src/erlydtl/``

  The source of the template compiler and runtime files. This is an
  adapted and extended version of ErlyDTL, which is an Erlang
  adaptation of the Django Template Language.

``zotonic/src/i18n/``

  Internationalization support. Currently, Zotonic supports basic
  multi-lingual websites. Full support for Internationalization is
  planned for the 1.0 release.

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
