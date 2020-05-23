.. _logon_submit:

logon_submit
^^^^^^^^^^^^

Handle a user logon. The posted query args are included. 
Return:: ``{ok, UserId}`` or ``{error, Reason}`` 


Type: 
    :ref:`notification-first`

Return: 
    

``#logon_submit{}`` properties:
    - query_args: ``list``
