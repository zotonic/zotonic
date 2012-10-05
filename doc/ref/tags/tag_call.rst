
.. index:: tag; call
.. _tag-call:

call
====

Call an Erlang function.

The ``{% call %}`` tag is used to call the ``render/2`` function of the module specified by the argument.

For example::

   {% call mymodule %}

Will call ``mymodule:render(TemplateVariables, Context)``.  Where `TemplateVariables` is the property list with all template variables and `Context` is the current Zotonic request :term:`context`.

The `render/2` function must return either ``{ok, IoList}`` or ``{error, Reason}``.

It is possible to pass a single value instead of the complete list of all template variables::

   {% call mymodule with value %}

This will call ``mymodule:render(Value, Context)``.

