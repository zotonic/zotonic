
.. include:: meta-mod_fileuploader.rst

Upload files using a parallel web worker.

This module provides a web worker and server logic to quickly upload files.

The web worker sends files in 128KB parts to the server, the server recombines all parts
to a single file. The web worker is using multiple parallel uploaders to make optimal use
of the available bandwidth.

Files are sent to the webworker on the topic ``model/fileuploader/post/new``:

.. code-block:: javascript

    let f = document.getElementById("upload_file").files[0];

    let msg = {
        files: [
            {
                name: "resultname",     // The name used in the ready message
                file: f                 // Must be a File object
            }
        ],
        ready_msg: {
            foo: "bar",                 // Anything you want to send if all files are uploaded
            files: []                   // This will be set by the worker
        },
        ready_topic: "bridge/origin/foo/bar",

        progress_msg: {
            form_id: "some-element-id",
            is_auto_unmask: true,
            percentage: 0               // Will be set by the uploader, 100 when ready
        },
        progress_topic: "zotonic-transport/progress"
    };

The files returned in the ``ready_mdg`` will be:

.. code-block:: javascript

    {
        name: "resultname",             // Name provided in the upload
        upload: "WZkhXoaMwrK2StUHmdpp"  // Unique name to use with z_fileuploader:status/1
    }

