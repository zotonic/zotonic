
.. index:: tag; translate
.. _tag-trans_ext:

trans (variable substitution)
=============================

Translate a text value using gettext and substitute variables.

Example::

    {% trans "Hello {foo} World" foo=1234 %}

This translates the text using the available po files. The variables in the translated text will
be substituted with the value of the arguments.

If the active language is “en” then the example above will output “Hello 1234 World”, in “nl” it will be “Hallo 1234 wereld”. Of course depending on the available translations. 

If a translation is not available then the text is output as-is without any translation.

If a “{” is needed in the string, then repeat it::

    {% trans "Hello {{foo}}, and this is {foo}." foo=1234 %}

Will echo “Hello {foo}, and this is 1234.”

.. seealso:: :ref:`tag-trans`.
