
.. index:: tag; call
.. _tag-call:

call
====

Call an Erlang function.

The ``{% call %}`` tag is used to call the ``render/3`` function of the module specified by the argument.

For example::

   {% call mymodule a=1 b=2 %}

Will call ``mymodule:render([{a,1}],[{b,2}], TemplateVariables, Context)``.  Where `TemplateVariables` is the property list with all template variables and `Context` is the current Zotonic request :term:`context`.

The `render/2` function must return either ``{ok, IoList}`` or ``{error, Reason}``.

If ``{error, Reason}`` is returned then the ``Reason`` is rendered as ``io_lib:format("~p", [Reason])``

For compatibility with Django it is possible to pass a single value instead an argument list::

   {% call mymodule with value %}

This will call ``mymodule:render(Value, TemplateVariables, Context)``.
