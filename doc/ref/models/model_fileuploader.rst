
.. include:: meta-fileuploader.rst

Model to start uploads, upload a block and delete uploads.

Topics
------

``model/fileuploader/post/new`` starts a new fileupload.

The posted message must include the filename and the file size::

    {
        filename: "test.jpg",
        size: 1000
    }

The upload URL and current status is returned::

.. code-block:: json

    {
        "result": {
            "filename": "test.jpg",
            "is_complete": false,
            "missing": [
                {
                    "size": 1000,
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



``model/fileuploader/post/delete/:name`` delete a fileupload.

``model/fileuploader/post/upload/:name/:offset`` upload data to a fileupload.

The offset must be an integer and the message payload must be binary data.

``model/fileuploader/get/status/:name`` fetch the current fileupload status.

