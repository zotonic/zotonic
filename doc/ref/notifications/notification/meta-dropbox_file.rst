.. _dropbox_file:

dropbox_file
^^^^^^^^^^^^

Handle a new file received in the 'files/dropbox' folder of a site. 
Unhandled files are deleted after an hour. If the handler returns 'ok' then 
the file is moved from the files/processing folder to files/handled. 
folder. 

Type: 
    :ref:`notification-first`

Return: 
    

``#dropbox_file{}`` properties:
    - filename: ``file:filename_all()``
    - basename: ``binary``
