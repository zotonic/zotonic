
.. include:: meta-file_id.rst

Redirect to the controller controller_file.

This controller maps a resource id to the filename of the medium associated with the resource.

For the redirect it uses the dispatch rule defined in the dispatch options.

Examples from mod_base::

   {media_inline, ["media","inline","id",id], controller_file_id, [ {dispatch, media_inline}, {ssl, any} ]},
   {media_inline, ["media","inline",'*'], controller_file, [ {content_disposition, inline}, {ssl, any} ]},

The first dispatch rule will redirect to the second.
If no associated file was found, then a 404 is returned.

.. seealso:: :ref:`controller-file`
