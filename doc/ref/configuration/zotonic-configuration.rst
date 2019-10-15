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

 - ``$HOME/.zotonic/(nodename@host)/``
 - ``$HOME/.zotonic/(nodename)/``
 - ``$HOME/.zotonic/(version)/``
 - ``$HOME/.zotonic/(major-version)/``
 - ``$HOME/.zotonic/``
 - ``/etc/zotonic/(nodename@host)/``
 - ``/etc/zotonic/(nodename)/``
 - ``/etc/zotonic/(version)/``
 - ``/etc/zotonic/(major-version)/``
 - ``/etc/zotonic/``

The ``(nodename)`` is the name of the Zotonic Erlang node, which
defaults to ``zotonic001`` (and can be set with ``$SNAME`` or ``$LNAME``
environment variable). Using the node name in the configuration path comes in
handy when you want to run multiple Zotonic instances simultaneously.

``(version)`` is the *minor* version number of Zotonic, e.g. ``1.0``. This
way, you can have separate configuration files for different versions of Zotonic
which are running simultaneously.

For example, if the version is 1.2 then ``(version)`` will be ``1.2`` and
``(major-version)`` will be ``1``.

If the Zotonic startup script finds a config file in one of the
directories, it stops looking, so files in the other directories are
ignored.

In the course of Zotonic starting up, it will print the locations of
the global config files that it is using:

.. code-block:: none

    12:42:17.351 [info] zotonic_launcher_sup:36 ================
    12:42:17.351 [info] zotonic_launcher_sup:37 Zotonic starting
    12:42:17.351 [info] zotonic_launcher_sup:38 ================
    12:42:17.351 [info] zotonic_launcher_sup:39 Init files used:
    12:42:17.356 [info] zotonic_launcher_sup:40 - /home/user/.zotonic/1/erlang.config
    12:42:17.356 [info] zotonic_launcher_sup:41 Config files used:
    12:42:17.357 [info] zotonic_launcher_sup:43 - /home/user/.zotonic/1/zotonic.config
    12:42:17.357 [info] zotonic_launcher_sup:44 ================

The used configuration files can be listed with ``bin/zotonic configfiles``:

.. code-block:: none

    user$ bin/zotonic configfiles
    Zotonic config files for zotonic@PoToi:
    - /home/user/.zotonic/1.0/erlang.config
    - /home/user/.zotonic/1.0/zotonic.config


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

    Zotonic config for zotonic@aloha:
    =================================

    zotonic:
        environment: production
        zotonic_apps: /home/user/zotonic/apps_user
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
for the :ref:`erlang-applications` that Zotonic depends on. Here you can
configure for instance the paths for the :ref:`log files <dev-testing>` (in
the ``lager`` section), emqtt ports, et cetera.

This file is included as an *init* configuration option when starting ``erl``
via the command line script in ``bin/zotonic``.

The erlang configuration van be viewed with ``bin/zotonic config erlang``:

.. code-block:: none

    Erlang init for zotonic@aloha:
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

