
.. index:: tag; _ (extended translation)
.. _tag-_:

_ (extended translation)
========================

Select a translation from the arguments.

The extended translation tag ``{% _ %}`` is used for specifying translations in the template itself.  This compared to the usual :ref:`tag-trans` tag ``{_ ... _}`` or ``{{ _"text" }}`` values which depends on separate gettext translations.

The arguments of the ``{% _ %}`` tag are the english text and the translations in different languages.

For example::

   {% _ "Example"  nl="Voorbeeld"  fr="Exemple" %}

.. seealso:: :ref:`tag-trans`.
