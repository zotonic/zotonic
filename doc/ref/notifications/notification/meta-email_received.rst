.. _email_received:

email_received
^^^^^^^^^^^^^^

Notification sent to a site when e-mail for that site is received 


Type: 
    :ref:`notification-first`

Return: 
    

``#email_received{}`` properties:
    - to: ``binary``
    - from: ``undefined|binary``
    - localpart: ``binary``
    - localtags: ``list``
    - domain: ``binary``
    - reference: ``binary``
    - email: ``record``
    - headers: ``list``
    - is_bulk: ``boolean``
    - is_auto: ``boolean``
    - decoded: ``unknown``
    - raw: ``unknown``
