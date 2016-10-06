.. _sanitize_embed_url:

sanitize_embed_url
^^^^^^^^^^^^^^^^^^

Sanitize an embed url. The hostpart is of the format: <<"youtube.com/v...">>. 


Type: 
    :ref:`notification-first`

Return: 
    ``undefined``, ``false`` or a binary with a acceptable hostpath

``#sanitize_embed_url{}`` properties:
    - hostpath: ``binary``
