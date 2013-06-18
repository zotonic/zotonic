
.. include:: meta-translation.rst

The m_translation model gives easy access to language and translation 
related information.

The following ``m.translation`` model properties are available in templates:

+----------------------+--------------------------------------+
|Property              |Description                           |
+======================+======================================+
|language              |The current language.                 |
+----------------------+--------------------------------------+
|language_list         |The list of all configured languages. |
+----------------------+--------------------------------------+
|language_list_enabled |The list of all enabled languages.    |
+----------------------+--------------------------------------+

.. highlight:: erlang

This is an example of the languages returned by ``m.translation.language_list``::

  [{en, [{is_enabled,true}, {language,<<"English">>}]},
   {fr, [{is_enabled,false}, {language,<<"Français">>}]},
   {nl, [{is_enabled,true}, {language,<<"Nederlands">>}]},
   {tr, [{is_enabled,true}, {language,<<"Türkçe">>}]}].

.. highlight:: django

For example to list all enabled languages in a select box::

  <select>
  {% for code,props in m.translation.language_list_enabled %}
    <option value="{{ code }}" {% if m.translation.language == code %}selected{% endif %}>{{ props.language }}</option>
  {% endfor %}
  </select>

