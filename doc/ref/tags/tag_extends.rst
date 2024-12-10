
.. index:: tag;  extends
.. _tag-extends:

extends
=======

.. seealso:: :ref:`tag-block`, :ref:`tag-inherit` and :ref:`tag-overrules`.

Inherit markup from another template.

.. note:: A template that extends another template contains only the extends tag and template :ref:`tag-block` tags.

Signal that this template extends another template. The extends tag
must be the first tag in a template that inherits from another
template.

Example:

.. code-block:: django

   {% extends "base.tpl" %}

All named template blocks in this template will replace the similar named template blocks in the template `base.tpl`.

Unlike Django the template name must be a string literal, variables are not allowed.
