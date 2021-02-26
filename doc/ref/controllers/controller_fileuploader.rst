
.. include:: meta-fileuploader.rst

The ``fileuploader`` controller is used to upload parts of a file.

For uploading data it accepts a POST with two arguments:

 * ``name`` is provided by the ``model/fileuploader/post/new`` method and uniquely identifies
   the file being uploaded.
 * ``offset`` provides the start point of the uploaded segment in the file. The size of
   the segment is derived from the uploaded data.

The body should be of ``application/octet-stream`` type with binary data.

The result is a JSON, describing the current status of the uploaded file.


Use a GET for fetching the status of an upload, only the name argument is needed.

.. code-block:: json

    {
        "result": {
            "filename": "test.jpg",
            "is_complete": false,
            "missing": [
                {
                    "size": 10,
                    "start": 0
                }
            ],
            "name": "WZkhXoaMwrK2StUHmdpp",
            "received": 0,
            "size": 10,
            "upload_url": "https://zotonic.test:8443/fileuploader/upload/WZkhXoaMwrK2StUHmdpp"
        },
        "status": "ok"
    }

Status ``"error"`` is returned if the name is unknown or any error occured.
