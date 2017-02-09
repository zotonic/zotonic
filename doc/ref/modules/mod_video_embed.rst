
.. include:: meta-mod_video_embed.rst

This module, if activated, checks the pasted URLs in the *create media / page*
dialog of the admin. It will show an embed option for Youtube and Vimeo URLs.
It will also cleanup pasted embed code for these and other services.

When used in the Zotonic site, the `{% media %}` tag then displays the
embed code.

This module is accompanies :ref:`mod_oembed` and can be
used for integrating with services that do not have oEmbed support
but do provide HTML embed-code functionality.

.. seealso:: :ref:`mod_oembed`, :ref:`mod_video`, :ref:`tag-media`
             
.. todo:: Add more documentation
