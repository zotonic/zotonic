.. _media_viewer_consent:

media_viewer_consent
^^^^^^^^^^^^^^^^^^^^

Optionally wrap HTML with external content so that it adheres to the cookie/privacy 
settings of the current site visitor. Typically called with a 'first' by the code that 
generated the media viewer HTML, as that code has the knowledge if viewing the generated code 
has any privacy or cookie implications. 


Type: 
    :ref:`notification-first`

Return: 
    {ok, HTML} or undefined

``#media_viewer_consent{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
    - consent: ``union``
    - html: ``iodata``
    - viewer_props: ``z_media_identify:media_info()``
    - viewer_options: ``list``
