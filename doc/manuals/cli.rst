.. _manual-cli:

The Zotonic shell command
=========================

The ``zotonic`` command runs a number of utility commands which all
operate on a Zotonic instance.

The ``zotonic`` command lives in the bin/ folder of the Zotonic
source. Putting this path into your PATH variable makes working with
Zotonic a lot easier::

  export PATH=$HOME/zotonic/bin:$PATH

The command determines where the Zotonic base dir is by looking at its path; it always assumes that its zotonic basedir is one dir up from where the binary itself is.

Currently, the following subcommands are implemented:

``zotonic start``
  Start the background Zotonic server instance.

``zotonic stop``
  Stop the background Zotonic server instance.

``zotonic debug``
  Launch the Zotonic server interactively and get an EShell on the running instance.  There is a "start.sh" command in the root folder which is a shortcut for this command.

``zotonic restart``
  Restart the background Zotonic server instance.

``zotonic update``
  Update the server.  Compiles and loads any new code, flushes caches and rescans all modules.

``zotonic addsite [options] <site_name>``
  Create a new site with [site_name] as its name.  This new site will be based on a so-called skeleton site. Currently there are two skeletons: 'blog' and 'empty'. "blog" is the default.

``zotonic addsite -s empty yoursite``
  creates a new site called "yoursite" based on the skeleton called "empty". It has the following options:

  -s <skel>    Skeleton site ('blog' or 'empty'; default: blog)

  -h <host>    Database host (default: 127.0.0.1)
  -p <port>    Database port (default: 5432)
  -u <user>    Database user (default: zotonic)
  -P <pass>    Database password (default: zotonic)
  -d <name>    Database name (default: zotonic)
  -n <schema>  Database schema (default: public)
  -a <pass>    Admin password (default: admin)
  -L           Create the site in the current directory instead of in `zotonic/priv/sites`.
  
  The "addsite" subcommand checks a file called
  $HOME/.zotonic-defaults for the default values to these
  options. This file is a file in bash-syntax which can define the
  following variables: ``SKEL``, ``DBHOST``, ``DBPORT``, ``DBUSER``, ``DBPASSWORD``,
  ``DBDATABASE``, ``DBSCHEMA``, ``ADMINPASSWORD``, ``DO_LINK``.

``zotonic installmodule <module_name>``
  Installs a module from the modules.zotonic.com repository into your Zotonic instance. The module will be checked out using source control (either git or hg) into the priv/modules folder::

    ~$ zotonic installmodule mod_openid
    Getting module index
    ** Installing mod_openid ...
    requesting all changes
    adding changesets
    adding manifests
    adding file changes
    added 11 changesets with 21 changes to 15 files
    updating to branch default
    14 files updated, 0 files merged, 0 files removed, 0 files unresolved
    ** mod_openid OK

``zotonic copysite [site_name] [source_server]``
  Copy [site_name] and its database content from the [source_server] over SSH and load its content into the filesystem and database of the local machine. You will need to have created the database zotonic_[site_name] for this to work.

  Warning: This command will reset the content of the database to the content retrieved from the [source_server].  It does, however, generate and output a restore file in case this was run by accident and explains how to recover.

``zotonic createdb [site_name]``
  Create a database called zotonic_[site_name] with the basic setup in place to host a Zotonic datastore. This script will likely need to be run as postgres unless zotonic has been granted CREATEDB in postgres as follows::

    ALTER ROLE zotonic WITH CREATEDB

``zotonic sitedir [site_name]``
  Get the absolute path for a site based on [site_name]

``zotonic snapshot [site_name]``
  Take a version control snapshot of [site_name] including its database content.

  This works differently from mod_backup in that it consistently uses
  the same filename for the SQL backup to make revision-based full
  site rollbacks possible.

``zotonic update``
  Update the server. Compiles and loads any new code, flushes caches and rescans all modules.

``zotonic compilefile [files...]``
  Compiles and reloads a single :term:`Erlang module` within the
  Zotonic folder. This runs very fast and works very well on a
  save-hook of your text editor. In Emacs, it would be called like
  this::

    (add-hook 'erlang-mode-hook
          '(lambda ()
             (add-hook 'after-save-hook '
                       (lambda ()
                         (call-process "/path/to/your/bin/zotonic" nil "*scratch*" nil "compilefile" buffer-file-name)
                         )
                       )
             ))
