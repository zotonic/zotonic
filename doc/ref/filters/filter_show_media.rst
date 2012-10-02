.. highlight:: django
.. include:: meta-show_media.rst

Convert the image markers in HTML from the richtext editor into image tags.

When you add images in the richtext editor of the Zotonic admin, the
HTML body text does not store <img> tags, but instead, stores special
markers containing the picture id and size&alignment hints.

The show_media tag converts these special markers (which are in fact
HTML comments) back into image tags. For this, it uses the template
called "_body_media.tpl". This template is located in mod_base and can
be overruled with your own.

Image which are inlined in the body text can have these parameters:

``alignment``
  choose between left, right and block.

``size``
  choose between "small", "middle", "large". The actual sizes that are
  taken come from the config key "site.media_dimensions", which
  defaults to "100x100,200x200,300x300".

``crop``
  checkbox if you want to crop your image to exactly the
  small/middle/large size. Otherwise it scales to fit in the bounding
  box of the respective size.  link - checkbox if you want to let the
  image include a link to its own page.

.. seealso:: :ref:`filter-without_embedded_media`
