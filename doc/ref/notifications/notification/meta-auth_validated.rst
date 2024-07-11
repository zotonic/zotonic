.. _auth_validated:

auth_validated
^^^^^^^^^^^^^^

Authentication against some (external or internal) service was validated 


Type: 
    :ref:`notification-first`

Return: 
    

``#auth_validated{}`` properties:
    - service: ``atom``
    - service_uid: ``binary``
    - service_props: ``map``
    - unknown: ``unknown``
    - identities: ``list``
    - ensure_username_pw: ``boolean``
    - is_connect: ``boolean``
    - is_signup_confirmed: ``boolean``
