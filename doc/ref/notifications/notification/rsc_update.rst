.. include:: meta-rsc_update.rst

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
        Map with resource properties.

``{ok, UpdateProps} | {error, Reason}``
    And/remove resource properties before the update is persisted.

``Context``
    Site context

Example
"""""""

Add a property before the resource is persisted::

    observe_rsc_update(#rsc_update{action = insert, id = Id}, {ok, Props}, Context) ->
        %% Set an extra property
        {ok, Props#{ <<"extra_property">> => <<"special value!">> }}.
    observe_rsc_update(#rsc_update{action = insert, id = Id}, {error, _} = Error, Context) ->
        Error.
