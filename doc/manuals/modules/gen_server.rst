.. _manual-modules-gen_server:

gen_server based modules
========================

When you need a running process, e.g., a module that does something in
the background, then it is possible to implement your module as a
gen_server. A gen_server is a standard way to implement a reliable
Erlang worker process.

In that case you will need to add the behaviour and
gen_server functions. You also need to change the ``init/1`` function to
accept an property list, which contains the site definition and a
``{context, Context}` property.

This server module will be started for every site in a Zotonic system
where the module is enabled, so it can’t be a named server.


.. seealso:: `Erlang gen_server principles <http://www.erlang.org/doc/design_principles/gen_server_concepts.html>`_

A minimal example
---------------------------------

.. literalinclude:: ex_module_gen_server.erl

As you can see, this code is almost identical to the standard Erlang
``gen_server`` boilerplate, with the exception of the metadata on top.

You also see that the ``start_link/1`` function is already
implemented. Note that in this function the gen_server is started
without registering the server under a name: this is done because the
module can be started multiple times; once for each site that needs
it.

The ``init/1`` function contains some more boilerplate for getting the
``context{}`` argument from the arguments, and storing this context
into the server's state. This way, you'll always have access to the
current context of the site in the rest of the gen_server's functions.
