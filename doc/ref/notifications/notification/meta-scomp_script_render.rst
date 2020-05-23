.. _scomp_script_render:

scomp_script_render
^^^^^^^^^^^^^^^^^^^

Add extra javascript with the {% script %} tag. (map) 
Used to let modules inject extra javascript depending on the arguments of the {% script %} tag. 
Must return an iolist() 


Type: 
    :ref:`notification-first`

Return: 
    

``#scomp_script_render{}`` properties:
    - is_nostartup: ``boolean``
    - args: ``list``
