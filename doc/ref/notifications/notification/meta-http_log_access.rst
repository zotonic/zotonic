.. _http_log_access:

http_log_access
^^^^^^^^^^^^^^^

Access log event for http. Called from the z_stats. 


Type: 
    :ref:`notification-notify_sync`

Return: 
    

``#http_log_access{}`` properties:
    - timestamp: ``erlang:timestamp()``
    - status: ``undefined|non_neg_integer``
    - status_category: ``xxx|1xx|2xx|3xx|4xx|5xx``
    - method: ``binary``
    - metrics: ``map``
