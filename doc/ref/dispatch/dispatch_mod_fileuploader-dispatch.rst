
.. include:: meta-mod_fileuploader-dispatch.rst

The ``fileuploader`` dispatch rule is used to upload parts of a file.

The ``name`` is provided by the ``model/fileuploader/post/new`` method and uniquely
identifies the file being uploaded.

The required ``offset=...`` query argument provides the start point of the
uploaded segment in the file. The size of the segment is derived from the uploaded
data.

The body should be of ``application/octet-stream`` type with binary data.

The result is a JSON, describing the current status of the uploaded file.
