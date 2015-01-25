.. highlight:: django
.. include:: meta-if.rst

Selects an argument depending on a condition.

For example::

  {{ value|if:"yes":"no" }}

This is a shortcut for using the :ref:`tag-if` tag. The same can be
expressed as follows::

  {% if value %}yes{% else %}no{% endif %}

Note that falsy values (0, ``false``, ``undefined`` or empty string) evaluate to false.

Elaborate examples
-----------------------

::

  {% with is_i18n|if:r.translation[lang_code].body:r.body as body %}

So if ``is_i18n`` evaluates to true, ``body`` is assigned to ``r.translation[lang_code].body``, else to ``r.body``.

::

  {% include "_language_attrs.tpl" id=pid class=(pid==id)|if:"active":"" %}

Add parameter ``class`` to the included template; when ``pid`` equals ``id``, ``class`` is ``"active"``, otherwise an empty string.



.. seealso:: :ref:`tag-if`, :ref:`filter-if_undefined`
