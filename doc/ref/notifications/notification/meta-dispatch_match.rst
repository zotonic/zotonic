.. _dispatch_match:

dispatch_match
^^^^^^^^^^^^^^

Final try for dispatch, try to match the request. 
Called when the site is known, but no match is found for the path 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, RscId::integer()}``, ``{ok, #dispatch_match{}}``, ``{ok, #dispatch_redirect{}}`` or ``undefined``

``#dispatch_match{}`` properties:
    - dispatch_name: ``atom``
    - mod: ``atom``
    - mod_opts: ``list``
    - path_tokens: ``list``
    - bindings: ``list``
