.. highlight:: django
.. include:: meta-name_unique.rst

A :ref:`validator <guide-validators>` to check whether a resource’s name is
unique::

    <input type="text" id="name" name="name" value="" />
    {% validate id="name" type={name_unique} %}

Optionally, pass an ``id`` parameter to exclude that particular id when testing
for uniqueness. This is useful when you want to exclude the name of the resource
currently being edited::

    <input type="text" id="name" name="name" value="" />
    {% validate id="name" type={name_unique id=id} %}

You can also pass a ``failure_message``::

    <input type="text" id="name" name="name" value="" />
    {% validate id="name" type={name_unique id=id failure_message=_"Eek! Already used!"} %}


.. seealso:: :ref:`guide-validators`, :ref:`validator-username_unique`
