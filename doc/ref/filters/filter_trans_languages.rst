.. include:: meta-trans_languages.rst

Return a list of all languages present in the given translated text (``#trans{}`` record).

If not translation is given, then the empty list is returned.

Example usage:

.. code-block:: none

    {% for iso in text|trans_languages %} {{ iso }} {% endfor %}

If the ``text`` has the value::

    #trans{ tr = [{en, <<>>}, {nl,<<"Hallo">>}] }

Then this will show ``en nl``.
