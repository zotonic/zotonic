.. index:: tag;  extends
.. _tag-extends:

extends
=======

Inherit markup from another template.

Signal that this template extends another template. The extends tag must be the first tag in a template that inherits from another template.

.. note:: A template that extends another template contains only the extends tag and block tags.

Example::

   {% extends "base.tpl" %}

All named blocks in this template will replace the similar named blocks in the template `base.tpl`.

Unlike Django the template name must be a string literal, variables are not allowed.

.. seealso:: :ref:`tag-block`, :ref:`tag-inherit` and :ref:`tag-overrules`.
