.. _ref-site-configuration:

Site configuration
==================

The ``config`` file is the central configuration file for your
site. Its syntax is the Erlang term format, essentially it is a big
`proplist` with different configuration options.

.. versionadded:: 0.10
   Each option must be specified at most once (per file).
   Using a option key multiple times are no longer supported (was only
   used by the `hostalias` option) and any duplicates will be silently
   dropped. Additional config options are read from files under
   ``config.d/``.

All files under the ``config.d/`` folder in the site’s folder are
loaded to extend/override config options from the site ``config``
file. So if the same key is present in both `config` and
``config.d/some-file``, then the value from the latter will be used.

The files under ``config.d/`` are read in alphabetical order.


The following options can be configured:

``{hostname, "127.0.0.1:8000"}``
  This config key specifies the hostname+port part of the site’s URL,
  to determine to which site an incoming request belongs to (since
  they all come in on the same port).

  If you run Zotonic on port 80, or if you put a web-frontend like
  varnish in front of your zotonic, you can leave out the port number,
  and just put the hostname in there. See the :ref:`guide-deployment`
  chapter on how to set up Zotonic for production environments.

  **Note:** The hostname does `not` specify on which port Zotonic will
  listen! That information comes from the ``ZOTONIC_PORT``
  environment variable, which, when absent, default to port 8000.
  Zotonic can (currently) listen on one TCP port for HTTP
  connections. For HTTPS, see the :ref:`mod_ssl` chapter.

``{protocol, "http"}``
  This is useful for when the Zotonic is running behind a proxy
  (like Varnish or haproxy) and the proxy translates between
  HTTPS (as seen by the browser) and HTTP (as seen by Zotonic).
  Setting this config key to "https" ensures that redirect locations
  have the correct HTTPS protocol.

.. versionadded:: 0.10
   The ``hostalias`` option now holds a list of aliases.

``{hostalias, ["www.example.com"]}``
  The host aliases allow you to specify extra aliases for your
  site. This comes in handy if you have registered yoursite.com,
  yoursite.net and yoursite.org, and all want them to be served the
  same site. Mind you that Zotonic will always redirect from a
  hostalias to the real hostname of the site. This is done to prevent
  content duplication: it is good web practice to let your content
  live on a single URL only.

  You can specify multiple host aliases; for that, just add the
  different `hostalias` options to the list::

    {hostalias, ["example.com", "www.example.com",
                 "example.net", "www.example.net"]},

``{admin_password, "test123"}``
  This setting specifies the password for the ``admin`` user. Unlike
  passwords for other users, the admin password is not stored in the
  database, but is set in the site’s config file.

``{depcache_memory_max, 100}``
  The maximum amount of memory a site may take. The `depcache` caches
  various results of function calls and database queries in memory. This
  setting determines the maximum size of it, in megabytes.

``{redirect, true}``
  Whether or not to redirect the host-aliases (listed by the
  ``hostalias`` directives) to the main hostname. This defaults to true.

``{skeleton, blog}``
  Set by the ``zotonic addsite`` command, this settings tells Zotonic
  which skeleton site to use.

``{install_menu, [<menu-item>...]}``
  Creates the initial main menu when installing mod_menu. A `menu-item`
  is a erlang tuple with a resource id and a list of child menu-items,
  if any: ``{rsc_name, []}``. This overrides the default menu provided
  by the skeleton.

``{install_modules, [<modules>...]}``
  List all modules that should be enabled when installing the site data.
  This overrides the default list of modules installed by the
  skeleton.

.. versionadded:: 0.16
   To inherit the list of modules from a skeleton, add a ``{skeleton,
   <name>}`` and it will install the list of modules from that skeleton
   as well.

  The list of installed modules will be updated on each site start,
  e.g. when you add a module to the ``install_modules`` list, it will
  be installed automatically when you restart the site.

``{ip_whitelist, "127.0.0.0/8,10.0.0.0/8,192.168.0.0/16,172.16.0.0/12,::1,fd00::/8"}``
  List of TCP/IP addresses and their netmasks.
  The admin user password *admin* will only be accepted if logging in
  from a host matching the whitelisted IP addresses. This for protecting
  development systems that are exposed to the Internet.
  This can also be configured in the :ref:`guide-configuration`.

``{smtphost, "..."}``
  Hostname you want e-mail messages to appear from. See :ref:`guide-email`.

``{websockethost, "..."}``
  The hostname that will be used for websocket requests. This hostname
  will be used in the browser for setting up the websocket connection.
  It can be used to configure a different port number for the websocket
  connection. For example::

    {websockethost, "example.com:443"}

``{cookie_domain, "..."}``
  The domain the Zotonic session-id and page-id cookies will be set
  on. Defaults to the main hostname.

.. versionadded:: 0.10

``{installer, <module>}``
  Override the default zotonic installer (``z_installer``). ``<module>`` should
  make sure that the database, if used, is setup properly along with any
  required data. Note that it is ``z_installer`` that is processing the
  ``install_modules`` and ``install_menu`` options, so if this module is not used
  then those menus and modules will not be installed unless the new module
  performs those operations.

``{service_api_cors, false}``
  See :ref:`guide-services-cors`.


.. _ref-site-configuration-database:

Database connection options
...........................

The following options for your site config specify how it connects to the database:

- dbhost
- dbport
- dbuser
- dbpassword
- dbdatabase
- dbschema
- dbdriver

These properties mostly speak for themselves, hopefully.

The `dbschema` is the name of the database schema (which is kind of a
namespace for tables in Postgres); see `Tip: multiple sites using one
database` below for an explanation. By default, ``public`` is used as
the schema name.

The `dbdriver` is the name of the database driver module. Currently
this defaults to ``z_db_pgsql``. Other driver options are not yet
implemented.


Setting module-specific config values in the site config
........................................................

It is also possible to add :ref:`model-config` values for modules to
the site's ``user/sitename/config`` file. To do this, add clauses like
this to the site's config::

  {mod_foo, [{key, value}, ...]}

For instance, to set the ``mod_ssl.is_secure`` configuration options
from :ref:`mod_ssl`, do::

  {mod_ssl, [{is_secure, true}]}


Reloading the site config
.........................

After you make changes to the site config you have to restart your
site for them to have effect. From the Zotonic shell, do::

  z_sites_manager:restart(yoursitename).

to restart your site.


Using environment variables in the site config
----------------------------------------------

Any variable in your site's ``config`` file can be retrieved from the
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

(note that the default value needs to be a string in this case, not an
int).


Tip: multiple sites using one database
--------------------------------------

In Zotonic, a single PostgreSQL database can host the data of multiple
web sites. This does not work using table prefixing (like Wordpress
does for example), but instead, Zotonic uses Postgres' native feature
`database schemas` to support this.

A database schema is basically another database inside your database:
it’s a namespace in which tables live. By default, your tables live in
the namespace called `PUBLIC`, but it’s quite easy to create another
schema::

  CREATE SCHEMA anothersite;
  GRANT ALL ON SCHEMA anothersite TO yourdatabaseuser;

And then in your site config put a ``{dbschema, "anothersite"}`` entry
next to the regular database config keys. Restart zotonic and off you
go.
