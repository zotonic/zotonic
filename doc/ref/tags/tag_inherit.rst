.. index:: tag;  inherit

.. _tag-inherit:

inherit
=======

Include the markup of an extended template into the extending template.


Say you have a template ``hello.tpl`` containing::
  
  {% block test %}
  This is content from hello.tpl
  {% endblock %}

And in your site you have a ``world.tpl`` template, defined as::

  {% extends "hello.tpl" %}
  {% block test %}
  First line
  {% inherit %}
  This is more content from world.tpl
  {% endblock %}

Then, the result of rendering the template ``world.tpl`` will be::

  First line
  This is content from hello.tpl
  This is more content from world.tpl

.. seealso:: :ref:`tag-block`, :ref:`tag-extends` and :ref:`tag-overrules`.
