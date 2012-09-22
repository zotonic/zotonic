Service URL::

  /api/base/media_upload


Request method(s):
  POST

Upload media items into Zotonic. Pass in the `file` argument for the
actual file. Because it's a file upload, the post payload should be
`multipart/form-data` encoded (which is the standard for file
uploads). Proper authorization is needed to use this API call, either
through session cookie or using OAuth. The value returned is a single
integer with the ID of the newly created media rsc.

Other arguments to this API call that can be passed in are: title,
summary, body, chapeau, subtitle, website, page_path.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/services/doc-media_upload.rst>`_
