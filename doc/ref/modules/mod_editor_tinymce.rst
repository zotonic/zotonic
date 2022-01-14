
.. include:: meta-mod_editor_tinymce.rst

Adds wysiwyg tinyMCE editors to the admin.

The module packs various versions, the version to be used can be selected on the ``/admin/modules`` page.

Two additional tinyMCE plugins are added:

 * The plugin ``zmedia`` for selecting images, videos and other media items. In a dialog the centering, size, and 
   optional caption can be choosen. The selected media are added to the page with a ``depiction`` connection.
 * The plugin ``zlink`` for adding links (``<a href="...">...</a>``) to Zotonic pages in the edited text. The title
   and URL to the page are automatically added.

Media are added as HTML comments in the body text. The filter :ref:`filter-show_media` is used to translate
the HTML comment into a figure or other tags. This is done using the template ``_body_media.tpl``, though
other templates can also be used.

Usage
-----

Use TinyMCE by incluyding ``_editor.tpl`` in your template.

This will, after page load, check all textarea tags with the class ``z_editor-init``. These textarea tags are then augmented with a TinyMCE editor.

Call the JavaScript function ``tinymce.triggerSave();`` to save all the TinyCME editor instances to
their textarea tags. A wired submit action will save before submitting the form.

Options
-------

All TinyMCE options are set in the global object ``tinyInit``.  This is done by the ``tiny-init.js`` file
inside the version of TinyMCE.  For example :file:`priv/lib/js/tinymce-5.10.2/tiny-init.js`

This file is included by ``editor.tpl``, which should be included in your template if you want to use the editor.

Supply your own ``tinyInit`` object to change the default settings. This can be done before the include
of ``_editor.tpl``, as the ``tiny-init.js`` will not replace any existing ``tinyInit`` object.


Stylesheet
----------

The default TinyMCE ``tiny_init.js`` included by ``_editor.tpl`` includes a stylesheet from
:file:`/lib/css/tinymce-zotonic.css`.

By adding a file :file:`priv/lib/css/tinymce-zotonic.css` in your site or module it is possible
to use your own styles for the editor.

Takes care of adding the styles for the images managed by the ``zmedia`` plugin.
The default image styles are:

.. code-block:: css

  img.z-tinymce-media {
      position: relative;
      border: 1px solid #667;
      height: auto;
      cursor: pointer;
      width: 270px;
  }

  img.z-tinymce-media.z-active:hover {
      border: 1px solid #039ed4;
  }

  img.z-tinymce-media-align-block {
      display: block;
      margin: 1rem 0;
      clear: both;
  }

  img.z-tinymce-media-align-right {
      float: right;
      margin: 0.5rem;
  }

  img.z-tinymce-media-align-left {
      float: left;
      margin: 0.5rem;
  }

  img.z-tinymce-media-size-large {
      width: 90%;
  }

  img.z-tinymce-media-size-middle {
      width: 50%;
  }

  img.z-tinymce-media-size-small {
      width: 25%;
  }


