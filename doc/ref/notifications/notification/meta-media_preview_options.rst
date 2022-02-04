.. _media_preview_options:

media_preview_options
^^^^^^^^^^^^^^^^^^^^^

Modify the options for an image preview url or tag. This is called for every 
image url generation, except if the 'original' image option is passed. The mediaclass 
in the options is not yet expanded. 


Type: 
    :ref:`notification-foldl`

Return: 
    modified property list of image options

``#media_preview_options{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
    - width: ``non_neg_integer``
    - height: ``non_neg_integer``
    - options: ``proplists:proplist()``
