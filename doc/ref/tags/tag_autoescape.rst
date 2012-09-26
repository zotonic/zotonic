
.. index:: tag; autoescape
.. _tag-autoescape:

autoescape
==========

Automatically apply HTML escaping to values.

The `autoescape` tag controls the current auto-escaping behavior. This tag takes either `on` or `off` as an argument and that determines whether auto-escaping is in effect inside the block.

When auto-escaping is in effect, all variable content has HTML escaping applied to it before placing the result into the output (but after any filters have been applied). This is equivalent to manually applying the escape filter to each variable.

Example::

   {{ value }}
   {% autoescape on %}
     {{ value }}
   {% endautoescape %}

When the variable value contains `<b>Hello</b>` then this will output::

   <b>Hello</b>
   &lt;b%gt;Hello&lt;/b&gt;
