.. _mailinglist_message:

mailinglist_message
^^^^^^^^^^^^^^^^^^^

Send a welcome or goodbye message to the given recipient. 
The recipient is either a recipient-id or a recipient props. 
'what' is send_welcome, send_confirm, send_goobye or silent. 


Type: 
    :ref:`notification-notify`

Return: 
    

``#mailinglist_message{}`` properties:
    - what: ``send_welcome|send_confirm|send_goodbye|silent``
    - list_id: ``m_rsc:resource()``
    - recipient: ``proplists:proplist()|integer``
