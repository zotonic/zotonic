.. highlight:: django
.. index:: tag; block
.. _tag-block:

template block
==============

.. seealso:: :ref:`tag-extends` and :ref:`tag-overrules`.

Define a template block and overrules a template block from an inherited template.

The `block` tag is used for replacing template blocks in inherited templates.

For example, when we have a template `base.tpl`, in which we define a template block called `name`::

   Hello {% block name %}my{% endblock %} world.

And we define a second template, `page.tpl`, which :ref:`tag-extends` the first template::

   {% extends "base.tpl" %}
   {% block name %}Peter's{% endblock %}

Then the result of rendering `page.tpl` will be::

   Hello Peter's world.

If we do not include the template block definition, so `page.tpl` just contains the :ref:`tag-extends` tag::

   {% extends "base.tpl" %}

then the output will be::

   Hello my world.

The name of a template block must be a valid identifier, consisting of
alphanumeric characters (a-z, 0-9) and the underscore charater.
