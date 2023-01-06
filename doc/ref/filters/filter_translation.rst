.. highlight:: django
.. include:: meta-translation.rst

.. seealso:: :ref:`filter-translate`

Lookup a specific translation in a translated text. If the text is not translated then
the text is returned as-is.

The filter takes as input a value or other variable and as argument the language to be shown.

Example usage::

    {{ text|translation:"en" }}

If the variable ``text`` has the value::

    {trans, [{en, <<"Hello">>}, {nl,<<"Hallo">>}]}

Then this will show ``Hello``, even if the current template language is set to ``nl``.

This filter is especially useful for filling in forms with language specific strings to be edited.
