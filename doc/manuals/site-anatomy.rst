.. _manual-site-anatomy:

Anatomy of a site
=================

Zotonic has the capability of serving more than one site at a
time. You can have multiple sites enabled, each with its own set of
templates, database and URL dispatch rules.

A Zotonic site is defined as a folder which lives in the priv/sites
directory of the zotonic installation (or in a location which is
symlinked to this folder, see the tip below).

A site's folder contains at least the following:

- a ``config`` file
- A file called ``sitename.erl``

Like a Zotonic module, the name of a Zotonic site must be valid as a
simple Erlang atom: it should be lowercase and only contain letters,
numbers and the underscore character.

Internally, sites are treated no different from a :ref:`Zotonic module
<manual-module-structure>`. In fact, they function in an identical
way, and as such, can contain all kinds of resources (templates,
dispatch rules, etc.) that a normal module also has. The only
difference is that site names do not need to be prefixed with `mod_`
and sites have an extra ``config`` file in their base directory. A
site's ``mod_prio`` metadata attribute is usually set to ``1``, to
make sure that it is the first module where Zotonic looks for template
lookups and the like.
  

The site config file
--------------------

The ``config`` file is the central configuration file for your
site. Its syntax is the Erlang term format, essentially it is a big
`proplist` with different configuration options.

The following options can be configured:

``{hostname, "127.0.0.1:8000"}``
  This config key specifies the hostname+port part of the site's URL,
  to determine to which site an incoming request belongs to (since
  they all come in on the same port).

  If you run Zotonic on port 80, or if you put a web-frontend like
  varnish in front of your zotonic, you can leave out the port number,
  and just put the hostname in there. See the :ref:`manual-deployment`
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

``{hostalias, "www.example.com"}``
  The host aliases allow you to specify extra aliases for your
  site. This comes in handy if you have registered yoursite.com,
  yoursite.net and yoursite.org, and all want them to be served the
  same site. Mind you that Zotonic will always redirect from a
  hostalias to the real hostname of the site. This is done to prevent
  content duplication: it is good web practice to let your content
  live on a single URL only.

  You can specify multiple host aliases; for that, just repeat the
  different `hostalias` options below each other::

    {hostalias, "example.com"},
    {hostalias, "www.example.com"},
    {hostalias, "example.net"},
    {hostalias, "www.example.net"},

``{admin_password, "test123"}``
  This setting specifies the password for the ``admin`` user. Unlike
  passwords for other users, the admin password is not stored in the
  database, but is set in the site's config file.

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
.. versionadded:: 0.10
   To inherit the list of modules from a skeleton, add a ``{skeleton,
   <name>}`` and it will install the list of modules from that skeleton
   as well.

``{smtphost, "..."}``
  Hostname you want e-mail messages to appear from. See :ref:`manual-email`.

``{streamhost, "..."}``
  The hostname that will be used for streaming comet/websocket
  requests. This hostname will be used in the browser for the stream
  connections instead of the main hostname, to circumvent browser
  limitations on the number of open sockets per host.

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


Database connection options
...........................

The following options for your site config specify how it connects to the database:

- dbhost 
- dbport
- dbuser
- dbpassword
- dbdatabase
- dbschema

 

Tip: using symlinks for easy development
--------------------------------------------

You clearly want to separate your new site from the main code
base. This migh seem hard because sites are defined as subfolders of
``priv/sites/`` inside the Zotonic code base.

However, using symlinks you can put your site anywhere on
your filesystem. For instance in ``$HOME/yoursite``. If you put it there,
just symlink it from the priv/sites directory like this::

  cd $HOME/zotonic/priv/sites
  ln -s $HOME/yoursite

When you now start Zotonic, everything will work as normal, and your
new site lives outside the repository. This way it is easy to put your
site under version control, for instance. The ``zotonic addsite``
command option ``-L`` automates this for you (see :ref:`manual-cli`).

Tip: multiple sites using one database
--------------------------------------

In Zotonic, a single PostgreSQL database can host the data of multiple
web sites. This does not work using table prefixing (like Wordpress
does for example), but instead, Zotonic uses Postgres' native feature
`database schemas` to support this.

A database schema is basically another database inside your database:
it's a namespace in which tables live. By default, your tables live in
the namespace called `PUBLIC`, but it's quite easy to create another
schema::

  CREATE SCHEMA anothersite;
  GRANT ALL ON SCHEMA anothersite TO yourdatabaseuser;

And then in your site config put a ``{dbschema, "anothersite"}`` entry
next to the regular database config keys. Restart zotonic and off you
go.

