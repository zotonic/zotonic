.. _ref-cli:

Command-line
============

The ``zotonic`` command runs a number of utility commands which all operate on a Zotonic instance.

.. tip::

    The ``zotonic`` command lives in the bin/ folder of the Zotonic
    source. Putting this path into your PATH variable makes working with
    Zotonic a lot easier::

        export PATH=$HOME/zotonic/bin:$PATH

The command determines where the Zotonic base dir is by looking at its path; it always assumes that its zotonic
basedir is one dir up from where the binary itself is.


Shell environment variables
---------------------------

See :ref:`environment variables <guide-deployment-env>` for the environment variables that can be set when
using the Zotonic command line tools.


Commands
--------

Currently, the following subcommands are implemented:

``zotonic start``
  Start the background Zotonic server instance.

``zotonic stop``
  Stop the Zotonic server instance.

``zotonic debug``
  Launch the Zotonic server interactively and get an EShell on the running instance. See :ref:`guide-cli-shell`.
  The ``start.sh`` command in the root folder is a shortcut for this command. The Zotonic instance can be stopped
  with twice ctrl-C.

``zotonic start_nodaemon``
  Start the Zotonic server instance as a foreground process, but without the interactive EShell. This is useful when
  running Zotonic in a Docker container or other process.

``zotonic restart``
  Restart the Zotonic server instance.

``zotonic wait [timeout]``
  Wait ``timeout`` seconds (defaults to 30 seconds) for Zotonic to be started, then return.

``zotonic shell``
  Get an EShell on the running Zotonic instance. See :ref:`guide-cli-shell`.

``zotonic status``
  List all sites on the running Zotonic instance and their current status.

``zotonic configfiles``
  List all config files used for Zotonic.

``zotonic configtest``
  Read all config files and check if they are syntactically correct.

``zotonic config [all | zotonic | erlang]``
  Prints the configuration as defined in the configuration files. Taking into account all shell environment variables.
  Defaults to showing the `zotonic` config.

``zotonic rpc``
  Send an RPC request to the running Zotonic instance. Example: `zotonic rpc "zotonic ping"`

``zotonic addsite [options] <site_name>``
  Creates a new site with [site_name] as its name.  See :ref:`guide-cli-addsite` for a full overview of this command.

``zotonic startsite <site_name>``
  Start the site with name [site_name].

``zotonic stopsite <site_name>``
  Stop the site with name [site_name].

``zotonic restartsite <site_name>``
  Restart the site with name [site_name].

``zotonic etop``
  Show the processes that consume the most CPU. Stop with twice ctrl-C.

``zotonic logtail``
  Show the last 50 entries of the ``console.log`` file.

``zotonic flush``
  Flush the caches of all sites.

``zotonic flush <site_name>``
  Flush the caches of the site with name [site_name].

``zotonic createdb <site_name>``
  Create a database called zotonic_[site_name] with the basic setup in place to host a Zotonic datastore.
  This script will likely need to be run as postgres unless zotonic has been granted CREATEDB in postgres as follows::

    ALTER ROLE zotonic WITH CREATEDB

``zotonic sitedir <site_name>``
  Get the absolute path for a site based on [site_name]

``zotonic compilefile <path/to/filename.erl>``
   Compiles and reloads a single :term:`Erlang module` within the
   Zotonic folder. This runs very fast and works very well on a
   save-hook of your text editor. In Emacs, it would be called like
   this:

.. code-block:: emacs

     (add-hook 'erlang-mode-hook
           '(lambda ()
              (add-hook 'after-save-hook '
                        (lambda ()
                          (call-process "/path/to/your/bin/zotonic" nil "*scratch*" nil "compilefile" buffer-file-name)
                          )
                        )
              ))

.. tip::
  Install ``fswatch`` or ``inotify-tools`` to automatically recompile files when they are changed. These tools will also
  enable automatic loading of changed templates, dispatch rules, and translations.

``zotonic compile``
  Compiles all the Zotonic Erlang source files, modules and sites,
  including those in the user directory (see :ref:`guide-configuration`).

``zotonic update``
  Like ``zotonic compile`` but also flushes caches and rescans all modules and sites for new templates etc.

``zotonic load``
  Reloads all (changed) beam files from disk.

``zotonic runtests``
  Starts Zotonic in the foreground and runs all (enunit) tests. Stops after completion of the tests.

``zotonic sitetest <site_name>``
  Runs all tests for the given site. Zotonic must be running. See :ref:`dev-testing`.

