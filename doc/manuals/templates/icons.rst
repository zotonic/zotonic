.. highlight:: django
.. _manual-tags: icons

Icons in templates
==================

Zotonic provides a couple of ways to show icons in templates:

* :ref:`mod_artwork` gives access to FontAwesome icons. It also has a number of other icon collections, mostly PNG images. Activate the module and follow the instructions on the doc page.
* Zotonic icons provided by `mod_base`. This is explained on the current page.


Zotonic icons
-------------

To create a certain amount of consistency across modules, Zotonic comes with a  small set of commonly used icons and CSS classes (edit, help, close, etcetera) plus the Zotonic logo.

Use cases:

* You create your frontend from scratch, but you also have pages in your site that are provided by other modules, for instance the login screens. It would be good if the social login icons show up. 
* You are writing a template or module and like to take advantage of ready available icons. 
* You are writing frontend styles in LESS and you would like to extend Zotonic or FontAwesome icons.


Including Zotonic icons CSS
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add the CSS file to your template::

    {% lib
        "css/z.icons.css"
    %}

This ensures that whenever a Zotonic icon class is used, the image will show up.


Writing Zotonic icons CSS classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In your template HTML, use this syntax::

    z-icon z-icon-<name>

For instance::

    <span class="z-icon z-icon-off"></span>

Always include ``z-icon``, except for the convenience class ``zotonic-logo``.

If you want to provide a text label for accessibility (but visually hidden), 
put the text inside ``<em>`` or ``<span>``::

    <span class="z-icon z-icon-off"><em>Log off</em></span>

Available classes::

    .z-icon-cross
    .z-icon-cross-circle
    .z-icon-drag
    .z-icon-edit
    .z-icon-facebook
    .z-icon-google-plus
    .z-icon-help
    .z-icon-help-circle
    .z-icon-info-circle
    .z-icon-instagram
    .z-icon-linkedin
    .z-icon-logo-z
    .z-icon-logo
    .z-icon-minus
    .z-icon-minus-circle
    .z-icon-off
    .z-icon-ok
    .z-icon-ok-circle
    .z-icon-plus
    .z-icon-plus-circle
    .z-icon-twitter
    .z-icon-user
    .zotonic-logo


Buttons
^^^^^^^

Icons-as-buttons: 2 convenience classes for commonly used buttons are::

    .z-btn-remove
    .z-btn-help

These buttons are styled as round buttons with size 16px. Button text inside ``<em>`` or ``<span>`` is hidden.

To have consistent close buttons, Bootstrap close buttons ``a.close`` and ``button.close`` are overridden by the ``z-btn-remove`` style.

Usage::

    <a class="z-btn-remove"><em>Remove</em></a>


Social login buttons
^^^^^^^^^^^^^^^^^^^^

Social login buttons are created with ``z-btn-social`` and show up with a brand icon at the side. Usage::

    <a href="#" class="btn z-btn-social"><span class="z-icon z-icon-facebook"></span> Login with Facebook</a>

NOTE: the full syntax that opens a new login window::

    <a href="{% url logon_service service='facebook' is_connect=is_connect %}" 
       class="btn z-btn-social do_popupwindow"
       data-popupwindow="height:300"
       style="color: white; background-color: #44609d">
          <span class="z-icon z-icon-facebook"></span>
          {% if is_connect %}
             {_ Connect with Facebook _}
          {% else %}
             {_ Login with Facebook _}
          {% endif %}
    </a>

"popupwindow" must be included in the template::

    {% lib
        "js/modules/z.popupwindow.js"
    %}


Writing LESS
^^^^^^^^^^^^

If you are writing frontend styles in LESS, Zotonic icons can be extended using mixins (found in ``extend.less``).

NOTE: The less files have a dependency with ``mod_artwork/lib/font-awesome-4``,
so you need to include the path to its LESS folder when using
the ``lessc`` command. For example::

    lessc --include-path="../../../mod_artwork/lib/font-awesome-4/less" 
    my_input.less my_output.css

Or for easier access, create a symlink to the `font-awesome-4` LESS folder
and add the symlink to the include-path.


Extending Zotonic icons
"""""""""""""""""""""""

To extend a class with a Zotonic icon class, write::

    .extend_icon(z, @name)

For example::

    .my-btn-help {
       .extend_icon(z, icon-help-circle);
    }

This will generate the following CSS::

    (lots-of-classes),
    .my-btn-help:before {
        speak: none;
        font-style: normal;
        font-weight: normal;
        font-variant: normal;
        text-transform: none;
        line-height: 1;
        display: inline-block;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
    }
    (lots-of-classes),
    .my-btn-help:before {
        font-family: "Zotonic";
    }
    .my-btn-help:before {
        content: "\e600";
    }

The ``:before`` pseudo-class can be extended to further style the icon. For instance to add a plus icon to a link::

    .my-plus-link {
        position: relative;
        padding-left: 16px;
    
        .extend_icon(z, icon-plus);
    
        &:before {
            position: absolute;
            top: 4px;
            left: 0;
            width: 16px;
            font-size: 13px;
        }
    }


Extending FontAwesome 4 icons
"""""""""""""""""""""""""""""

1. Enable module mod_artwork.

2. In LESS, add parameter 'fa' to the `extend` mixin::

     .btn-bookmark {
         .extend_icon(fa, fa-var-bookmark);
     }

The icon (variable) names can be found in ``mod_artwork/lib/font-awesome-4/less/variables.less``.

3. To load the webfonts, add the CSS to the template, for instance: ``{% lib "font-awesome-4/css/font-awesome.min.css" %}``
