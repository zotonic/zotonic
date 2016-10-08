.. _auth_autologon:

auth_autologon
^^^^^^^^^^^^^^

Check if automatic logon is enabled for this session. Sent for new 
sessions from ``z_auth:logon_from_session/1``. Please note this notification 
is sent for every single request. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, UserId}`` when a user should be logged on.

``#auth_autologon{}`` properties:
none