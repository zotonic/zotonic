
.. include:: meta-admin_edit_widget_i18n.rst

i18n-enabled blocks
-------------------   

* widget_content
* widget_i18n_tab_class

Introduction
------------

More complex widget for editing language-depend content.  Look at
:ref:`mod_admin` before start to develop i18n-enabled widgets.

Workflow
--------

Depending on whether mod_translation enabled or disabled:

* if enabled, this widget will be displayed as tabs. See /admin/edit/1 top;
* if disabled - will be displayed as standard non-localized widget.

i18n variables
--------------

To provide by-language differentiation several variables are used in i18n-blocks:

+----------------------------+------------------------------------------------------+-------------------+
|Param name                  |                                                      | Example           |
|                            |                  Description                         | value             |
+============================+======================================================+===================+
|is_i18n                     | Shows whether mod_translation enabled or not.        | true              |
|                            | Used to implement basic logic for displaing current  |                   |
|                            | rsc values:                                          |                   |
|                            | {{ if:is_i18n|if : r.translation[lang_code].summary  |                   |
|                            |    : r.title }}                                      |                   |
|                            | You should read it this way: "if mod_translation     |                   |
|                            | enabled, then display localized title, else display  |                   |
|                            | non-localized title.                                 |                   |
+----------------------------+------------------------------------------------------+-------------------+
|lang_code                   | ISO-2 language code. This code is defined in         | "en"              |
|                            | "traslation" tab for each language.                  |                   |
|                            | This variable is used to read current value into     |                   |
|                            | field:                                               |                   |
|                            | m.rsc[id].r.translation[lang_code].title             |                   |
+----------------------------+------------------------------------------------------+-------------------+
|lang_code_with_dollar       | Same as lang_code, but contains dollar sign at       | "$en"             |
|                            | beginning. It is used for ids and names of localized |                   |
|                            | form elements. On submit, resource_admin_edit will   |                   |
|                            | process following localized $-names:                 |                   |
|                            | <input name="myparam{{ lang_code_with_dollar }}" ... |                   |
+----------------------------+------------------------------------------------------+-------------------+
|lang_code_with_brackets     | Same as lang_code, but usually used for displayed    | "(en)"            |
|                            | comments, titles, lables, etc. This variable may be  |                   |
|                            | safety replaced by code:                             |                   |
|                            | ({{ lang_code }})                                    |                   |
+----------------------------+------------------------------------------------------+-------------------+
| is_editable                | Whether or not current rsc is editable for user.     | true              |
+----------------------------+------------------------------------------------------+-------------------+

Notes
-----

Only blocks "widget_content" and "widget_i18n_tab_class" provide
localization. They are will be rendered several times: one time per
every enabled language. In other words, rendered once per lang
tab. All other blocks are rendered only once - when page loads.
