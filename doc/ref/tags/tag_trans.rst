.. highlight:: django
.. index:: tag; translate
.. _tag-trans:

translate
=========

.. seealso:: :ref:`tag-trans_ext`.

Translate a text value using gettext.

Translate the text contained in the tag into the currently selected language.

Example::

   {_ translate me _}

If the active language is “nl” then this will output “vertaal mij”.  Of course depending on the available translations.

If a translation is not available then the text is output as-is without any translation.

