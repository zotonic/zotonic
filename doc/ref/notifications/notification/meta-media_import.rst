.. _media_import:

media_import
^^^^^^^^^^^^

Notification to translate or map a file after upload, before insertion into the database 
Used in mod_video to queue movies for conversion to mp4. 
You can set the post_insert_fun to something like fun(Id, Medium, Context) to receive the 
medium record as it is inserted. 


Type: 
    :ref:`notification-first`

Return: 
    modified ``#media_upload_preprocess{}``

``#media_import{}`` properties:
    - url: ``binary``
    - host_rev: ``list``
    - unknown: ``unknown``
    - metadata: ``tuple``
