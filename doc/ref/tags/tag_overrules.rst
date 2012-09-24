.. index:: tag; overrules
.. _tag-overrules:

overrules
=========

Inherit markup from like named template in another module.

Signal that this template extends a template with the same name in a module with lower priority.

The `overrules` tag must be the first tag in the template.

.. note:: A template that overrules another template contains only the :ref:`tag-extends` tag and :ref:`tag-block` tags.

Example, say a template "page.tpl" contains the following::

   {% overrules %}
   {% block title %} My new title {% endblock %}

All named blocks in this template will replace the similar named blocks in the template `page.tpl` that is "next in line" to be used.

This is useful if you want to use a template from a module, and the template is mentioned in (for example) a dispatch rule. Now you can overrule and extend that template in your own modules without changing the dispatch rules or the original module.

Make sure your module has a higher priority (lower number) than the module containing the overruled template.

.. seealso:: :ref:`tag-block`, :ref:`tag-inherit` and :ref:`tag-extends`.
