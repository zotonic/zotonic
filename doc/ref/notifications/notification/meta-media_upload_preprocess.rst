.. _media_upload_preprocess:

media_upload_preprocess
^^^^^^^^^^^^^^^^^^^^^^^

Notification to translate or map a file after upload, before insertion into the database 
Used in mod_video to queue movies for conversion to mp4. 
You can set the post_insert_fun to something like fun(Id, Medium, Context) to receive the 
medium record as it is inserted. 


Type: 
    :ref:`notification-first`

Return: 
    modified ``#media_upload_preprocess{}``

``#media_upload_preprocess{}`` properties:
    - id: ``integer|insert_rsc``
    - mime: ``binary``
    - file: ``file:filename()``
    - original_filename: ``file:filename()``
    - medium: ``list``
    - post_insert_fun: ``function``
