.. _translate:

translate
^^^^^^^^^

Request a translation of a list of strings. The resulting translations must 
be in the same order as the request. This notification is handled by modules 
that interface to external translation services like DeepL or Google Translate. 
Return {ok, List} | {error, Reason} | undefined. 


Type: 
    :ref:`notification-first`

Return: 
    

``#translate{}`` properties:
    - from: ``atom``
    - to: ``atom``
    - texts: ``list``
