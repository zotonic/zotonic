.. _ref-site-configuration:

Site configuration
------------------

This chapter describes the configuration options for your sites. There’s also
:ref:`global configuration <guide-configuration>`.

Site config locations
^^^^^^^^^^^^^^^^^^^^^

Configuration for sites is stored:

- in a ``priv/zotonic_site.config`` file in the site directory
- optionally, in files in the site’s ``priv/config.d/`` directory. This is so that
  automated provisioning tools can easily override site configuration.

The ``config.d`` files will extend and/or override the configuration options
from the ``zotonic_site.config`` file. So if the same key is present in both
``zotonic_site.config`` and ``config.d/some-file``, the value from ``some-file``
will be used. The files under ``config.d/`` are read in alphabetical order.

Parameters
^^^^^^^^^^

.. important::

    After changing any of these configuration parameters,
    :ref:`restart the site <restart-site>` for the change to take effect.

admin_password
""""""""""""""

The password for the admin user::

    {admin_password, "top_secret"},

Note that if the *admin_password* is set to ``admin`` then only clients with
local network addresses can logon as the admin. See also ``ip_allowlist`` below.

.. _ref-site-configuration-database:

dbhost
""""""

Database host that the site connects to. Example::

    {dbhost, "127.0.0.1"},

Defaults to the ``dbhost`` in the Zotonic config.
Which defaults to ``127.0.0.1``.

dbport
""""""

Port of the database server. Example::

    {dbport, 5432},

Defaults to the ``dbport`` in the Zotonic config.
Which defaults to ``5432``.

dbuser
""""""

Database user. Defaults to the ``dbuser`` in the Zotonic config.
Which defaults to ``zotonic``.

dbpassword
""""""""""

Database password. Defaults to the ``dbpassword`` in the Zotonic config.
Which defaults to ``zotonic``.


dbdatabase
""""""""""

Database name. Use ``none`` to run Zotonic without a database::

    {dbdatabase, none},

Defaults to the database configured in the Zotonic config (which defaults to ``zotonic``).

If the ``dbuser`` does not have permission to create the database then you have to create it
yourself:

.. code-block:: sql

    CREATE DATABASE "some_database" ENCODING = 'UTF8' TEMPLATE template0;
    GRANT ALL ON DATABASE some_database TO some_db_user;

Where *some_db_user* must be the user you configured with ``dbuser`` and *some_database* must be
the database configured with ``dbdatabase``.


dbschema
""""""""

PostgreSQL Database schema. Defaults to either:

 * the name of the site if the ``dbdatabase`` is the default database from the zotonic config
 * ``public`` if the ``dbdatabase`` is set to something else than the default from the Zotonic config.

So if you configure a special database for your site then the default schema will be ``public``.

Example::

    {dbschema, "mysiteschema"},

In Zotonic, a single PostgreSQL database can host the data of multiple sites.
This does not work using table prefixing, but instead, Zotonic uses PostgreSQL’s
native feature of database schemas.

A database schema is basically another database inside your database. If your dbuser does not
have permission to create the schema, then you will need to create any schema other than ``public`` first:

.. code-block:: sql

    CREATE SCHEMA some_site;
    GRANT ALL ON SCHEMA some_site TO some_db_user;

Where *some_db_user* must be the user you configured with ``dbuser``. After creating the schema, either
restart the site or restart Zotonic.


db_max_connections
""""""""""""""""""

Maximum number of database connections used by the site. Unused connections are
closed after some time.

This parameter should be enlarged if the following error occures frequently::

    Database pool usage is close to exhaustion, please increase the pool size.

And definitely increase the pool size if this error occurs frequently::

    Database pool is exhausted, please increase the pool size.

The default is 20 connections::

    {db_max_connections, 20},


depcache_memory_max
"""""""""""""""""""

The maximum amount of memory a site may take (in MB). The depcache caches
various results of function calls and database queries in memory. Example::

    {depcache_memory_max, 100},


hostname
""""""""

The hostname and port part of the site URL. This is used to determine to which
site an incoming request should be routed. Example::

    {hostname, "127.0.0.1"},

Note that the hostname does *not* specify on which port Zotonic will listen;
this is :ref:`configured globally <ref-port-ssl-configuration>`.

hostalias
"""""""""

A list of alias hostnames for the site. By default, Zotonic redirects these
to ``hostname`` (see ``redirect``). Example::

    {hostalias, [
        "example.com",
        "www.example.com",
        "example.net",
        "www.example.net"
    ]},

.. _site-configuration-protocol:

redirect
""""""""

Whether or not to redirect the host-aliases (listed by the ``hostalias``
directives) to the main hostname. Defaults to ``true``, to prevent
content duplication: it is good web practice to let your content live on a
single URL only::

    {redirect, true},

skeleton
""""""""

Set by the ``zotonic addsite`` command, this settings tells Zotonic
which skeleton site to use. Example::

    {skeleton, blog},

install_menu
""""""""""""

Creates the initial main menu when installing :ref:`mod_menu`. A menu item
is an Erlang tuple with a resource name and list of child menu items (if any):
``{name, []}``.
Example::

    {install_menu, [
        {page_some_thing, []},
        {page_some_other_thing, []},
        {page_one_more_thing, []}
    ]},

.. _site-configuration-modules:

modules
"""""""

List of all modules that are :ref:`activated <activating-modules>` when the
site is started. After adding a module here, :ref:`restart the site <restart-site>`
to load the module. Example::

    {modules, [
        mod_admin,
        mod_menu,
        mod_your_custom_module
    ]},


ip_allowlist
""""""""""""

List of TCP/IP addresses and their netmasks. The default admin user password
(“admin”) will only be accepted for an IP in the allowlist. This protects
development systems that are exposed to the internet. This can also be
configured :ref:`globally <guide-configuration>`. Default::

    {ip_allowlist, "127.0.0.0/8,10.0.0.0/8,192.168.0.0/16,172.16.0.0/12,::1,fd00::/8"}

smtphost
""""""""

Hostname you want e-mail messages to appear from. See :ref:`guide-email`.

cookie_domain
"""""""""""""

The domain the Zotonic session-id and page-id cookies will be set on. Defaults
to the main hostname.

installer
"""""""""

Override the default zotonic installer (``z_installer``). ``<module>`` should
make sure that the database, if used, is setup properly along with any
required data. Note that it is ``z_installer`` that is processing the
``install_modules`` and ``install_menu`` options, so if this module is not used
then those menus and modules will not be installed unless the new module
performs those operations. Example::

    {installer, your_installer_erlang_module},

Setting module-specific config values in the site config
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is also possible to add :ref:`model-config` values for modules to
the site's ``sitename/priv/zotonic_site.config`` file. To do this, add
clauses like this to the site's config::

    {mod_foo, [{key, value}, ...]}


Using environment variables in the site config
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Any variable in your site's ``zotonic_site.config`` file can be retrieved from the
OS environment variables. To do so, wrap the config value in a ``{env,
...}`` tuple. For instance, to use the ``DB_HOST`` environment
variable as the database host, put the following as the ``dbhost``
config value::

    {dbhost, {env, "DB_HOST"}},

Besides ``{env, "NAME"}`` tuple, you can also specify ``{env, "NAME",
"default value"}`` for the case the environment variable is not set::

    {dbhost, {env, "DB_HOST", "localhost"}},

To convert environment variables to integer (e.g. for the database
port), use ``env_int``::

    {dbhost, {env_int, "DB_PORT"}},

or, with default value::

    {dbhost, {env_int, "DB_PORT", "5432"}},

Note that the default value needs to be a string in this case, not an int.
