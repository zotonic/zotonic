.. _media_identify_file:

media_identify_file
^^^^^^^^^^^^^^^^^^^

Try to identify a file, returning a map with file properties. 


Type: 
    :ref:`notification-first`

Return: 
    map with binary keys, especially ``<<"mime">>``, ``<<"width">>``, ``<<"height">>``, ``<<"orientation">>``

``#media_identify_file{}`` properties:
    - filename: ``file:filename_all()``
    - original_filename: ``binary``
    - extension: ``binary``
