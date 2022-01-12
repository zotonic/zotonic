
.. include:: meta-language.rst
.. seealso:: :ref:`filter-language_dir`, :ref:`filter-is_rtl`

Return the language the resource (or translated text) will be displayed in.

Example::

    {{ id|language }}

The languages of the resource will be fetched and using the currently selected
interface language (variable ``z_language``) the language for the resource to
be displayed in will be returned.
