
.. index:: tag; block
.. _tag-block:

block
=====

Overrule a block from an inherited template.

The `block` tag is used for replacing blocks in inherited templates.

For example, when we have a template `base.tpl`::

   Hello {% block name %}my{% endblock %} world.

And we render the template::

   {% extends "base.tpl" %}
   {% block name %}Peter's{% endblock %}

Then the output will be::

   Hello Peter's world.

Though when we render the template::

   {% extends "base.tpl" %}

then the output will be::

   Hello my world.

The name of a block must be a valid identifier.

.. seealso:: :ref:`tag-extends` and :ref:`tag-overrules`.
