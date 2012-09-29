
.. index:: tag;  inherit

.. _tag-inherit:

inherit
=======

Include the markup of a "super" template into the overridden template.


Say you have a module containing ``hello.tpl``::
  
  {% block test %}
  This is content from the module
  {% endblock %}

And in your site you override ``hello.tpl`` as::

  {% block test %}
  {% inherit %}
  This is content from the site
  {% endblock %}

Then, the result of rendering the template will be::

  This is content from the module
  This is content from the site

.. seealso:: :ref:`tag-block`, :ref:`tag-extends` and :ref:`tag-overrules`.
