.. _media_import_props:

media_import_props
^^^^^^^^^^^^^^^^^^

Notification to translate or map a file after upload, before insertion into the database 
Used in mod_video to queue movies for conversion to mp4. 
You can set the post_insert_fun to something like fun(Id, Medium, Context) to receive the 
medium record as it is inserted. 


Type: 
    :ref:`notification-first`

Return: 
    modified ``#media_upload_preprocess{}``

``#media_import_props{}`` properties:
    - prio: ``pos_integer``
    - category: ``atom``
    - module: ``atom``
    - description: ``binary|tuple``
    - rsc_props: ``list``
    - medium_props: ``list``
    - medium_url: ``binary``
    - preview_url: ``binary``
