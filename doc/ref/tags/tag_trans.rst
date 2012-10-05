
.. index:: tag; translate
.. _tag-trans:

translate
=========

Translate a text value using gettext.

Translate the text contained in the tag into the currently selected language.

Example::

   {_ translate me _}

Alternative representation::

   <!--{_ translate me _}-->

If the active language is “nl” then this will output “vertaal mij”.  Of course depending on the available translations. When a translation is not available then the text is output as-is without any translation.

.. seealso:: :ref:`tag-_`.

