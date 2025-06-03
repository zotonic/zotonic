.. highlight:: django
.. include:: meta-page_path_unique.rst
.. seealso:: :ref:`guide-validators`, :ref:`validator-username_unique`, :ref:`validator-name_unique`

A :ref:`validator <guide-validators>` to check whether a resource’s page path is
unique::

    <input type="text" id="page_path" name="page_path" value="">
    {% validate id="page_path" type={page_path_unique} %}

Optionally, pass an ``id`` parameter to exclude that particular id when testing
for uniqueness. This is useful when you want to exclude the page paths of the resource
currently being edited::

    <input type="text" id="page_path" name="page_path" value="">
    {% validate id="page_path" type={page_path_unique id=id} %}

You can also pass a ``failure_message``::

    <input type="text" id="page_path" name="page_path" value="">
    {% validate id="page_path" type={page_path_unique id=id failure_message=_"Eek! Already used!"} %}
