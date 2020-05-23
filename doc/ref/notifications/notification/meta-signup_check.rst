.. _signup_check:

signup_check
^^^^^^^^^^^^

signup_check 
Check if the signup can be handled, a fold over all modules. 
Fold argument/result is {ok, Props, SignupProps} or {error, Reason} 


Type: 
    :ref:`notification-foldl`

Return: 
    ``{ok, Props, SignupProps}`` or ``{error, Reason}``

``#signup_check{}`` properties:
    - props: ``list``
    - signup_props: ``list``
