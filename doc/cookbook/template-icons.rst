Icons in templates
==================

Zotonic provides a couple of ways to show icons in templates:

* :ref:`mod_artwork` gives access to FontAwesome and Material Design icons.
  It also has a number of other icon collections, mostly PNG images. Activate
  the module and follow the instructions on the doc page.
* Zotonic icons provided by `mod_base`. This is explained on the current page.

To create a certain amount of consistency across modules, Zotonic comes with a
small set of commonly used icons and CSS classes (edit, help, close, etcetera)
plus the Zotonic logo.

Use cases:

* You create your frontend from scratch, but you also have pages in your site
  that are provided by other modules, for instance the login screens. It would
  be good if the social login icons show up.
* You are writing a template or module and like to take advantage of ready
  available icons.
* You are writing frontend styles in LESS and you would like to extend Zotonic
  / FontAwesome / Material Design icons.

Include the Zotonic icons CSS file in your template::

    {% lib
        "css/z.icons.css"
    %}

Then use this syntax in your template HTML::

    z-icon z-icon-<name>

For instance::

    <span class="z-icon z-icon-off"></span>

.. seealso:: :ref:`ref-icons` reference

