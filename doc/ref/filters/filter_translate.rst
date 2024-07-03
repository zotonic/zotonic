.. highlight:: django
.. include:: meta-translate.rst

.. seealso:: :ref:`filter-translation`

Translates a (English) value to the current language or the given language.

If the input is a ``#trans{}`` record then it is extended with the translations
from the .po files before the language lookup is done.  For this the ``#trans{}``
record *must* have the English translation.

Example with the default language lookup (accessible via the template variable ``z_language``)::

  {{ "Cancel"|translate }}

If the current language is ``de`` then the output is: ``"Abbrechen"``.

An example with a specific language::

  {{ "Cancel"|translate:"nl" }}

The output would be ``"Annuleer"``.
