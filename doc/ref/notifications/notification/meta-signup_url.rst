.. _signup_url:

signup_url
^^^^^^^^^^

Handle a signup of a user, return the follow on page for after the signup. 
Return ``{ok, Url}`` 
'props' is a proplist with properties for the person resource (email, name, etc) 
'signup_props' is a proplist with 'identity' definitions and optional follow on url 'ready_page' 
An identity definition is {Kind, Identifier, IsUnique, IsVerified} 


Type: 
    :ref:`notification-first`

Return: 
    

``#signup_url{}`` properties:
    - props: ``list``
    - signup_props: ``list``
