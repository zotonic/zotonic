.. _manual-cli:

The zotonic command
===================

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
  Launch the Zotonic server interactively and get an EShell on the running instance. See :ref:`manual-cli-shell`. The ``start.sh`` command in the root folder is a shortcut for this command.

``zotonic restart``
  Restart the background Zotonic server instance.

``zotonic update``
  Update the server.  Compiles and loads any new code, flushes caches and rescans all modules.
  
``zotonic shell``
  Get an EShell on the running instance. See :ref:`manual-cli-shell`.

``zotonic addsite [options] <site_name>``
  Creates a new site with [site_name] as its name.  See :ref:`manual-cli-addsite` for a full overview of this command.

``zotonic modules <subcommand> [options]``
  Manages modules. It has the following subcommands:
  
  ``install <module> [module2, ...]``  Installs a module from the http://modules.zotonic.com repository into your Zotonic instance. The module will be checked out using source control (either git or hg) into the priv/modules folder.

  ``uninstall <module> [module2, ...]``  Uninstall a module

  ``activate <module> [module2, ...]``  Activate a module

  ``deactivate <module> [module2, ...]``  Deactivate a module

  ``update <module> [module2, ...]``  Update a module

  ``restart <module> [module2, ...]``  Restart a module

  ``reinstall <module> [module2, ...]``  Reinstall a module

  ``list``  List all modules available on the Zotonic Module Repository

  ``search <query>``  Search for a module

  subcommand options:

  --version     show program's version number and exit
  -h, --help    show this help message and exit
  -z ZMR, --zmr=ZMR  Zotonic modules repository
  -s SITE, --site=SITE  affected Zotonic site
  -d, --debug   enable debugging
  -n NODE, --node=NODE  Zotonic Erlang node
  

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

``zotonic compile``
  Compiles all the Zotonic Erlang source files, modules and sites,
  including those in the user sites directory and user modules
  directory (see :ref:`manual-configuration`). This command is mainly
  called from the Makefile when building Zotonic. It does *not*
  compile Zotonic's dependencies (the Erlang files under the ``deps/``
  folder). This command can only be run when Zotonic is not running; for hot code reloads, use ``zotonic update``.
  
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


``zotonic logtail``
  Starts a ``tail -F`` on the three Zotonic log files, console.log, error.log and crash.log

