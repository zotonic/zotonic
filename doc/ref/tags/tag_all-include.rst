.. index:: tag; all include
.. _tag-all-include:

all include
===========

Call all modules to include a certain template.

This is an extension on the :ref:`tag-include` tag. It will include all templates with the given name, instead of the first one found.  Templates are defined in modules, because of that multiple modules can define a template with the same name.

For example when you have two modules (`mod_a` and `mod_b`), both with the template `_name.tpl`.  When the template in `mod_a` is defined as::

   this is mod_a's {{ hello }}

and in mod_b as::

   this is mod_b's {{ hello }}

then the tag::

   {% all include "_name.tpl" hello="world" %}

Will output::

   this is mod_a's world
   this is mod_b's world

The modules will be called in the order of their defined priority. This is the order in which they are listed in the module admin page.

Examples of this mechanism can be found in :ref:`mod_admin`, for example the main menu and the category specific editing fields on the edit page.

Another example is the `_html_head.tpl` template which is included from the :ref:`template-base` template and allows all modules to add HTML to the head of a generated HTML page.

.. seealso:: tag :ref:`tag-include`.
