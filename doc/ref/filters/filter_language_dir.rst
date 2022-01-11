.. highlight:: django
.. include:: meta-language_dir.rst

Return ``rtl`` or ``ltr`` depening on the direction of the language.

Example::

    <div dir="{{ z_language|language_dir }}">
        {_ Text _}
    </div>

Input can also be an resource (page) id. In that case the language that the
resource is shown in will be used to determine the direction.

Example::

    <div lang="{{ id|language }}" dir="{{ id|language_dir }}">
        {{ id.body }}
    </div>

It currently returns ``rtl`` for Arabic (``ar``), Farsi (``fa``)
and Hebrew (``he``).

.. seealso:: :ref:`filter-language`, :ref:`filter-is_rtl`
