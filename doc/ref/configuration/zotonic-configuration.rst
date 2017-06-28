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

 - ``$HOME/.zotonic/(nodename)/``
 - ``$HOME/.zotonic/(version)/``
 - ``$HOME/.zotonic/(major-version)/``
 - ``$HOME/.zotonic/``
 - ``/etc/zotonic/(nodename)/``
 - ``/etc/zotonic/(version)/``
 - ``/etc/zotonic/(major-version)/``
 - ``/etc/zotonic/``

The ``(nodename)`` is the name of the Zotonic Erlang node, which
defaults to ``zotonic001`` (and can be set with ``$NODENAME`` environment
variable). Using the node name in the configuration path comes in
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

    17:03:54.766 [info] Zotonic started
    17:03:54.766 [info] ===============
    17:03:54.766 [info] Config files used:
    17:03:54.768 [info] - /home/user/.zotonic/1.0/erlang.config
    17:03:54.768 [info] - /home/user/.zotonic/zotonic001/zotonic.config


The ``zotonic.config`` file
^^^^^^^^^^^^^^^^^^^^^^^^^^^

After installed for the first time, the ``~/.zotonic/zotonic.config`` file is well
annoted with comments about what each setting does. When in doubt,
consult the stock ``apps/zotonic_launcher/priv/config/zotonic.config.in`` file for
explanation about all config settings.


``user_sites_dir``, ``user_modules_dir``
""""""""""""""""""""""""""""""""""""""""

Zotonic keeps sites and modules that are `external` to Zotonic, e.g.
installed by website developers, outside the Zotonic source tree.

The directory under which Zotonic expects to find all sites is called
the :term:`User sites directory`. This is configured with the config
parameter ``user_sites_dir``. This directory defaults to
``_checkouts``, relative to Zotonic's installation directory.

The directory under which Zotonic expects to find all external
modules, e.g. those installed with ``zotonic modules install
mod_....``, is called the User modules directory. This is configured
with the config parameter ``user_modules_dir``. This directory
defaults to ``_checkouts``, relative to Zotonic's installation
directory.

.. _deps:

`deps`
......

The ``zotonic.config`` file can hold extra dependencies which are to
be installed as part of a user's installation. These deps are
formatted similar to how they would be listed in a ``rebar.config`` file::

   {deps,
    [
     {jsx, "1.4", {git, "git://github.com/talentdeficit/jsx", {tag, "v1.4"}}}
    ]},


On compile time, these deps are added to the list of the standard deps
from Zotonic's ``rebar.config`` file, and cloned and compiled in the
same way.

.. _erlang-config:

The `erlang.config` file
^^^^^^^^^^^^^^^^^^^^^^^^

The ``erlang.config`` file contains application environment variables
for the :ref:`erlang-applications` that Zotonic depends on. Here you can
configure for instance the paths for the :ref:`log files <dev-testing>` (in
the ``lager`` section), emqtt ports, et cetera.
