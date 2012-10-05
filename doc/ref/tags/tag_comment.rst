
.. index:: tag; comment
.. _tag-comment:

comment
=======

Ignore part of a template.

Everything inside a ``{% comment %}`` block is not output.

Example::

   This will show.
   {% comment %} And this will not show {% endcomment %}

This will output::

   This will show.

An alternative to the ``{% comment %}`` tag is to use the ``{# ... #}`` construct::

   This will show.
   {# And this will not show #}

The big advantage of this notation is that the contents of the ``{# ... #}`` construct don't need to be grammatically correct, as they will not be parsed.  The contents of  a ``{% comment %}`` block must be correct as they will be parsed by the template compiler.

