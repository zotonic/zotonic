.. include:: includes/meta-rsc_update_done.rst

``pre_is_a``
    List of resource categories before the update.
``post_is_a``
    List of resource categories after the update.
``pre_props``
    List of properties before the update.
``post_props``
    List of properties after the update.

Example
"""""""

Add some default edges when a resource is created::

    observe_rsc_update_done(#rsc_update_done{action = insert, id = Id, post_is_a = PostIsA, post_props = Props}, Context) ->
        case lists:member(activity, PostIsA) of
            false ->
                ok;
            true ->
                m_my_rsc:create_default_edges(Id, Context),
                ok
        end;
    observe_rsc_update_done(#rsc_update_done{}, _Context) ->
        %% Fall through
        ok.

