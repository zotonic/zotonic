Module dependencies
===================

Modules can have dependencies on other modules. These are expressed
via the module's metadata, as follows::

    -mod_depends([mod_admin]).

This states that the current module is dependent on ``mod_admin`` to
be installed.

Sometimes, explicitly depending on a module name is not a good idea:
there might be more modules that perform the same functions but are
competing in implementation. In that case, such modules can export a
``mod_provides`` meta tag, so that dependent modules can `depend` on
what one of these modules `provide`.

Example: ``mod_a`` and ``mod_b`` both provide some functionality, ``foo``::

  -module(mod_a).
  -mod_provides([foo]).

and::
  
  -module(mod_b).
  -mod_provides([foo]).

Now, another module, ``mod_bar``, needs the "foo" functionality::

  -module(mod_bar).
  -mod_depends([foo]).

Now, the module manager will require either (or both!) of the
``mod_a`` and ``mod_b`` modules to be activated, before ``mod_bar``
can be activated.
