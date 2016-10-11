.. include:: includes/meta-rsc_update.rst

An updated resource is about to be persisted. Observe this notification to
change the resource properties before they are persisted.

Arguments
"""""""""

``#rsc_update``
    ``action``
        Either ``insert`` or ``update``.
    ``id``
        Id of the resource.
    ``props``
        List of resource properties.

``{IsChanged, UpdateProps}``
    And/remove resource properties before the update is persisted. Set
    ``IsChanged`` to ``true`` if you want to modify ``UpdateProps``.

``Context``
    Site context

Example
"""""""

Add a property before the resource is persisted::

    observe_rsc_update(#rsc_update{action = insert, id = Id}, {Modified, Props}, Context) ->
        %% Set an extra property
        {true, Props ++ [{extra_property, <<"special value!">>}].
