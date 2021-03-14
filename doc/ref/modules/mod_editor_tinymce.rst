
.. include:: meta-mod_editor_tinymce.rst

Adds wysiwyg tinyMCE editors to the admin.

The module packs various versions, the version to be used can be selected on the ``/admin/modules`` page.

Two additional tinyMCE plugins are added:

 * ``zmedia`` for selecting images, videos and other media items. In a dialog the centering, size, and 
   optional caption can be choosen. The selected media are added to the page with a ``depiction`` connection.
   
 * ``zlink`` for adding links (``<a href="...">...</a>``) to Zotonic pages in the edited text. The title
    and URL to the page are automatically added.

Media are added as HTML comments in the body text. The filter :ref:`filter-show_media` is used to translate
the HTML comment into a figure or other tags. This is done using the template ``_body_media.tpl``, though
other templates can also be used.
