.. _import_resource:

import_resource
^^^^^^^^^^^^^^^

An external feed delivered a resource. First handler can import it. 
Return:: ``{ok, m_rsc:resource_id()}``, ``{error, Reason}``, or ``undefined`` 


Type: 
    :ref:`notification-first`

Return: 
    

``#import_resource{}`` properties:
    - source: ``atom|binary``
    - source_id: ``integer|binary``
    - source_url: ``binary``
    - source_user_id: ``binary|integer``
    - user_id: ``integer``
    - name: ``binary``
    - props: ``m_rsc:props_all()``
    - urls: ``list``
    - media_urls: ``list``
    - data: ``any``
