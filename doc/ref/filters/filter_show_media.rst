.. highlight:: django
.. include:: meta-show_media.rst

Convert the image markers in HTML from the Rich Text editor into image tags.

When you add images in the Rich Text editor of the Zotonic admin, the
HTML body text does not store ``<img>`` tags, but instead, special
markers containing the picture id plus size and alignment hints.

The ``show_media`` tag converts these special markers (which are in fact
HTML comments) back into image tags. For this, it uses the template
called ``_body_media.tpl``. This template is located in ``mod_base`` and can
be overruled with your own.

Images that are inlined in the body text can have these parameters:

``alignment``
  Choose 'between', 'left' and 'right'.

``size``
  Choose between "small", "middle", "large". The actual sizes that are
  taken come from the config key "site.media_dimensions", which
  defaults to "200x200,300x300,500x500".

``crop``
  Checkbox if you want to crop your image to exactly the
  small/middle/large size. Otherwise it scales to fit in the bounding
  box of the respective size.

``link``
  Checkbox if you want to let the image include a link to its own page.

These parameters can be set in the editor dialog "Insert a Zotonic media item".

.. seealso:: :ref:`filter-without_embedded_media`
