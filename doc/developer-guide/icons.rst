.. _ref-icons:

Icons
=====

Including Zotonic icons CSS
---------------------------

Add the CSS file to your template::

    {% lib
        "css/z.icons.css"
    %}

This ensures that whenever a Zotonic icon class is used, the image will show up.

Writing Zotonic icons CSS classes
---------------------------------

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
       data-popupwindow='{ "height": 300 }'
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

NOTE: The less files have a dependency with ``zotonic_mod_artwork/priv/lib/font-awesome-4``,
so you need to include the path to its LESS folder when using
the ``lessc`` command. For example::

    lessc --include-path="../../../../zotonic_mod_artwork/priv/lib/font-awesome-4/less"
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

The ``:before`` pseudo-class can be extended to further style the icon. For instance to add a plus icon to a link:

.. code-block:: text

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


Extending Material Design icons
"""""""""""""""""""""""""""""""

1. Enable module mod_artwork.

2. In LESS, add parameter 'md' to the `extend` mixin and pass the character code::

     .btn-bookmark {
         .extend_icon(md, "\f019");
     }

The icon (variable) characters can be found in the `icons cheatsheet <http://zavoloklom.github.io/material-design-iconic-font/cheatsheet.html>`_.



Extending FontAwesome 4 icons
"""""""""""""""""""""""""""""

1. Enable module mod_artwork.

2. In LESS, add parameter 'fa' to the `extend` mixin::

     .btn-bookmark {
         .extend_icon(fa, fa-var-bookmark);
     }

The icon (variable) names can be found in ``zotonic_mod_artwork/priv/lib/font-awesome-4/less/variables.less``.
