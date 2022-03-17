.. _rsc_import_fetch:

rsc_import_fetch
^^^^^^^^^^^^^^^^

Fetch the data for an import of a resource. Returns data in the format 
used by m_rsc_export and m_rsc_import. 


Type: 
    :ref:`notification-first`

Return: 
    {ok, map()} | {error, term()} | undefined

``#rsc_import_fetch{}`` properties:
    - uri: ``binary``
