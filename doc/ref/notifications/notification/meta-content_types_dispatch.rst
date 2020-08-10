.. _content_types_dispatch:

content_types_dispatch
^^^^^^^^^^^^^^^^^^^^^^

Get available content types and their dispatch rules 
Example: {"text/html", page} 
A special dispatch rule is 'page_url', which refers to the page_url property of the resource. 


Type: 
    :ref:`notification-foldr`

Return: 
    ``[{ContentType, DispatchRule}]``

``#content_types_dispatch{}`` properties:
    - id: ``m_rsc:resource()``
