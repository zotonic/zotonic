.. _manual-configuration:

Global configuration
====================

This section describes the location and contents of Zotonic's `global`
configuration files.


Config file locations
---------------------

Zotonic depends on two global config files, called ``zotonic.config``
and ``erlang.config``. On startup, Zotonic looks in the following
places for these files:

 - ``$HOME/.zotonic/(nodename)/``
 - ``$HOME/.zotonic/(version)/``
 - ``$HOME/.zotonic/``
 - ``/etc/zotonic/(nodename)/``
 - ``/etc/zotonic/(version)/``
 - ``/etc/zotonic/``

Where `(nodename)` is the name of the Zotonic Erlang node, which
defaults to `zotonic001` (and can be set with `$NODENAME` environment
variable).

`(version)` is the `minor` version number of Zotonic, e.g. ``0.11``
for all Zotonic ``0.11.x`` variants. This way, you can have separate
configuration files for different versions of Zotonic which are
running simultaneously.
   
When the Zotonic startup script finds a config file in one of the
directories, it stops looking, so files in the other directories are
ignored.


The `zotonic.config` file
---------------------------

user_sites_dir and user_modules_dir
...................................

Since version 0.11, Zotonic keeps sites and modules that are
`external` to Zotonic, e.g. installed by website developers, outside
the Zotonic source tree.

The directory under which Zotonic expects to find all sites is called
the User sites directory. This is configured with the config parameter
``user_sites_dir``.

The directory under which Zotonic expects to find all external
modules, e.g. those installed with ``zotonic modules install
mod_....``, is called the User modules directory. This is configured
with the config parameter ``user_modules_dir``.



  
The `erlang.config` file
--------------------------

The ``erlang.config`` file contains application environment variables
for the Erlang applications that Zotonic depends on. Here you can
configure for instance the paths for the log files (in the ``lager``
section), emqtt ports, et cetera.
