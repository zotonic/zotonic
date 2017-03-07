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

``align``
  Either ``block``, ``left`` or ``right``. Default is ``block``.

``size``
  Choose between ``small``, ``medium``, ``large``. These are used
  for selecting the mediaclass of the image. Which will be like
  ``body-media-large``. The default size is ``large``, displayed
  as a block (see align above).

``crop``
  Checkbox if you want to force cropping of the image to the bounding box
  of the mediaclass. If checked then the crop in the mediaclass is overruled.
  If not checked then the crop in the mediaclass is used.

``link``
  Checkbox if you want to let the image include a link to its own page.

``caption``
  Caption that will be displayed below the media item. Per default the summary
  of the media item is used for the caption. If the caption is empty, then no
  caption is added.

These parameters can be set in the editor dialog ‘Media Properties’.
This dialog is shown after clicking on an image in the wysiwyg editor.

.. image:: /img/show_media_properties.png


Template arguments
------------------

The ``_body_media.tpl`` is included using a *catinclude* for the media item.
In this way you can switch templates depending on the item category being
displayed.

You can add your own ``_body_media.tpl`` templates. It will be supplied with
the following data:

+--------------------------+----------------------------------------------+
|Argument                  |Description                                   |
+==========================+==============================================+
|id                        |The id of the medium item to be displayed.    |
+--------------------------+----------------------------------------------+
|size                      |The size, ``small``, ``medium`` or ``large``  |
+--------------------------+----------------------------------------------+
|mediaclass                |Mediaclass, for example ``body-media-large``  |
+--------------------------+----------------------------------------------+
|align                     |Alignment, ``block``, ``left`` or ``right``   |
+--------------------------+----------------------------------------------+
|crop                      |If crop is forced, ``true`` or ``undefined``  |
+--------------------------+----------------------------------------------+
|link                      |If the image should link to the medium page   |
+--------------------------+----------------------------------------------+
|caption                   |The caption from the editor                   |
+--------------------------+----------------------------------------------+

Besides the above all context variables are passed, this gives the Erlang code
the possibility to change the behavior of the media rendering.


.. seealso::
    :ref:`filter-embedded_media`, :ref:`filter-without_embedded_media`
