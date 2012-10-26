.. highlight:: erlang

Activate/deactivate modules
===========================
Rescuing a dysfunctional site from the Zotonic shell.

Why
---

Sometimes it happens that disabled or enabled a module by accident and
your whole site is now dysfunctional. You can easily rescue your site,
from the Erlang command line.

How
---

First launch the Zotonic EShell::

  ~/zotonic/bin/zotonic shell 

On the Zotonic EShell: Deactivate a module::

  z_module_manager:deactivate(mod_modulename, z:c(yoursitename)).

On the Zotonic EShell: Activate a module::

  z_module_manager:activate(mod_modulename, z:c(yoursitename)). 

Or do both of these, when you want to restart your module::

  Context = z:c(yoursitename),
  z_module_manager:deactivate(mod_modulename, Context),
  z_module_manager:activate(mod_modulename, Context).

Where `mod_modulename` is the name of your module and `yoursitename` is the name of your site.

Check Your Hostname
-------------------

If you can't launch the shell chances are that your hostname is not a
fully-qualified domain name (FQDN).  This complicates things since
Erlang is picky about allowing connections to unqualified nodes. For
example, the hostname should be ``myprimarysite.example.com`` rather than
something random and local like ``coolserver``.
