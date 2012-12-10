.. _manual-extensions:
.. _highlight: erlang

Extensions
==========

Zotonic has an extra mechanism for starting additional things that
need to be running under the ``zotonic_sup`` supervisor. These are
called `extensions`. Extensions are not tied to any particular Zotonic
site, but are regular Erlang processes, in OTP fashion.

On the startup of Zotonic, the global ``$ZOTONIC/priv/extensions`` folder is
scanned once for folders starting with ``ext_``. Each of these folders
is considered an `extension`.

An extension (for instance, ``ext_foo/ext_foo.erl``) can be a regular
Erlang module, but is supposed to be something supervisable like a
gen_server. It needs to expose at least a function
``start_link/0``. On Zotonic startup, this function is called and the
resulting pid is added to the Zotonic supervision tree as a `worker` process.

