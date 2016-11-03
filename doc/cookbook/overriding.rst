.. _cookbook-overriding:

Overriding Zotonic
==================

This chapter describes how to override the templates, styling and logic
provided by Zotonic.

Overriding works by adding a site or module that has a higher
:ref:`priority <module-priority>` than Zotonic’s built-in modules. In your
module/site, you add templates, assets and create notification observers.

Overriding templates
--------------------

:ref:`Override templates <overriding-templates>` by adding a template with
the same name to your module/site.

.. seealso:: :ref:`cookbook-error-pages`

Overriding assets
-----------------

If you wish to fully override a CSS or JavaScript file, do so in the same way
as you do templates: create a file with the same name in your module/site.
Alternatively, *add* your own CSS file and selectively override CSS styles.

Overriding logic
----------------

:ref:`Observe notifications <guide-notifications-observe>` to influence the
decisions that Zotonic makes. You can change or add properties before a resource
is persisted,

