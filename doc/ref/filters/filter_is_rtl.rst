.. highlight:: django
.. include:: meta-is_rtl.rst

.. seealso:: :ref:`filter-language_dir`, :ref:`filter-language`

Check if the given language is a rtl or ltr language.

Example::

  {% if z_language|is_rtl %}
      You are browsing in an RTL language
  {% endif %}

It currently returns ``true`` only for Arabic (``ar``), Farsi (``fa``)
and Hebrew (``he``).

