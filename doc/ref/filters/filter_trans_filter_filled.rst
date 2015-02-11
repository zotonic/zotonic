.. highlight:: django
.. include:: meta-trans_filter_filled.rst

Filters all empty translations from a property.

This is used if it is important to show a text, but not all translations are filled in.

The filter takes as input a resource or other variable and as argumnent the property to be shown.

Example usage::

    {{ id|trans_filter_filled:`body` }}

.. highlight:: erlang

If the resource ``id`` has the body property::

    {trans, [{en, <<>>}, {nl,<<"Hallo">>}]}

Then this will show ``Hallo``, even if the language is set to ``en``.
