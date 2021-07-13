.. _guide-configuration:

Global configuration
--------------------

This section describes the location and contents of Zotonic’s global
configuration files ``erlang.config`` and ``zotonic.config``. There’s also
:ref:`site-specific configuration <ref-site-configuration>`.

Config file locations
^^^^^^^^^^^^^^^^^^^^^

Zotonic depends on two global config files, called ``zotonic.config``
and ``erlang.config``. On startup, Zotonic looks in the following
places for these files:

 * The init argument ``zotonic_config_dir``
 * The environment variable ``ZOTONIC_CONFIG_DIR``
 * The directory :file:`$HOME/.zotonic`
 * The directory :file:`/etc/zotonic` (only on Unix)
 * The OS specific directory for application config files

The OS specific directories are:

 * On Unix: :file:`~/.config/zotonic/config/`
 * On macOS: :file:`~/Library/Application Support/zotonic/config/`

In those directories the system searches for a ``zotonic*`` file in the following subdirectories (assuming the version of Zotonic is 1.2.3 and the node is called ``zotonic001@foobar``):

 * ``zotonic001@foobar/``
 * ``zotonic001/``
 * ``1.2.3/``
 * ``1.2/``
 * ``1/``
 * ``.``

The default is the OS specific directory, with as subdirectory the major version number of Zotonic (in this case ``1``).
For Linux this would be :file:`~/.config/zotonic/config/1/`

The nodename is the name of the Zotonic Erlang node, which
defaults to ``zotonic001`` (and can be set with ``$SNAME`` or ``$LNAME``
environment variable). Using the node name in the configuration path comes in
handy when you want to run multiple Zotonic instances simultaneously.

By checking for *version* you can have separate configuration files for different versions of Zotonic
which are running simultaneously.

If the Zotonic startup script finds a ``zotonic*`` file in one of the
directories, it stops looking, so files in the other directories are
ignored.

Zotonic will also load all configuration files in the ``config.d`` directory inside the Zotonic directory.

In the course of Zotonic starting up, it will print the locations of
the global config files that it is using:

.. code-block:: none

    12:42:17.351 [info] zotonic_launcher_sup:36 ================
    12:42:17.351 [info] zotonic_launcher_sup:37 Zotonic starting
    12:42:17.351 [info] zotonic_launcher_sup:38 ================
    12:42:17.351 [info] zotonic_launcher_sup:39 Init files used:
    12:42:17.356 [info] zotonic_launcher_sup:40 - /home/user/.config/zotonic/config/1/erlang.config
    12:42:17.356 [info] zotonic_launcher_sup:41 Config files used:
    12:42:17.357 [info] zotonic_launcher_sup:43 - /home/user/.config/zotonic/config/1/zotonic.config
    12:42:17.357 [info] zotonic_launcher_sup:44 ================

The used configuration files can be listed with ``bin/zotonic configfiles``:

.. code-block:: none

    user$ bin/zotonic configfiles
    Zotonic config files for zotonic001@foobar:
    - /home/user/.config/zotonic/config/1.0/erlang.config
    - /home/user/.config/zotonic/config/1.0/zotonic.config


The ``zotonic.config`` file
^^^^^^^^^^^^^^^^^^^^^^^^^^^

After installed for the first time, the ``~/.zotonic/1/zotonic.config`` file is well
annotated with comments about what each setting does. When in doubt, consult the
stock ``apps/zotonic_launcher/priv/config/zotonic.config.in`` file for explanation
about all config settings.

In the ``zotonic.config`` file you will find the password for the
``zotonic_status`` site where you can manage the server.

Zotonic configurations can also be fetched in the Erlang shell.
For example, view the ``zotonic_status`` password::

  z_config:get(password).

The Zotonic configuration files are read by the ``zotonic_launcher`` application before
starting the core zotonic applications and all sites.

The zotonic configuration van be viewed with ``bin/zotonic config``:

.. code-block:: none

    Zotonic config for zotonic001@aloha:
    ====================================

    zotonic:
        environment: production
        zotonic_apps: /home/user/zotonic/apps_user
        security_dir: /home/user/.config/zotonic/security
        password: Bthj3ruGbmgJxfmc
        timezone: UTC
        listen_ip: any
        listen_ip6: any
        listen_port: 8000
        ssl_listen_port: 8443
        port: 80
        ssl_port: 443
        max_connections: 20000
        ...


.. _erlang-config:

The `erlang.config` file
^^^^^^^^^^^^^^^^^^^^^^^^

The ``erlang.config`` file contains application environment variables
for the Erlang applications that Zotonic depends on. Here you can
configure for instance the paths for the :ref:`log files <dev-testing>` (in
the ``lager`` section), emqtt ports, et cetera.

This file is included as an *init* configuration option when starting ``erl``
via the command line script in ``bin/zotonic``.

The erlang configuration van be viewed with ``bin/zotonic config erlang``:

.. code-block:: none

    Erlang init for zotonic001@aloha:
    =================================

    exometer:
        predefined:
          - {[erlang,memory],{function,erlang,memory,[],value,[]},[]}
          - {[erlang,system_info],
             {function,erlang,system_info,['$dp'],value,[process_count]},
             []}
          - {[erlang,statistics],
             {function,erlang,statistics,['$dp'],value,[run_queue]},
             []}
          - {[erlang,io],
             {function,erlang,statistics,[io],match,{{'_',input},{'_',output}}},
             []}
    filezcache:
        data_dir: priv/filezcache/data
        journal_dir: priv/filezcache/journal
    lager:
    ...

