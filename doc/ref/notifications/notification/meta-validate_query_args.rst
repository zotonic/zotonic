.. _validate_query_args:

validate_query_args
^^^^^^^^^^^^^^^^^^^

Called just before validation of all query arguments by z_validation. 
     This is the moment to filter any illegal arguments or change query 
     arguments. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``{ok, list( {binary(), z:qvalue()} )} | {error, term()}``

``#validate_query_args{}`` properties:
none