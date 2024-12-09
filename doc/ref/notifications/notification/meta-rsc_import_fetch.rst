.. _rsc_import_fetch:

rsc_import_fetch
^^^^^^^^^^^^^^^^

Fetch the data for an import of a resource. Returns data in the format 
used by m_rsc_export and m_rsc_import. Either returns the JSON data, the 
imported resource id, or the resource id and a map with a mapping from URIs to 
resource ids. 


Type: 
    :ref:`notification-first`

Return: 
    {ok, map()} | {ok, m_rsc:resource_id()} | {ok, {m_rsc:resource_id(), map()}} | {error, term()} | undefined

``#rsc_import_fetch{}`` properties:
    - uri: ``binary``
