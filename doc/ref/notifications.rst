.. index:: notifications

.. _notifications reference:

Notifications
=============

This is a list of the most important built-in notifications that Zotonic sends.
Observe these notifications in your code to add custom functionality. All
notifications are defined as records in ``include/zotonic_notifications.hrl``.

acl_is_allowed
--------------

Check if a user is authorized to perform an operation on a an object. Observe
this notification to do complex or more fine-grained authorization checks than
you can do through the ACL rules admin interface.

Arguments
.........

``#acl_is_allowed``
    ``action``
        ``view``, ``update``, ``delete`` or ``use``
    ``object``
        resource id or a module name (in case of ``use``)

``Context``

Return value
............

``false``
    disallow operation: the default
``true``
    allow operation
``undefined``
    refrain from voting and let the next observer decide

Example
.......

Deny anyone from viewing unpublished resource except those who have update
rights on the resource (usually the creator and the administrator)::

    observe_acl_is_allowed(#acl_is_allowed{action = view, object = Id}, Context) ->
        case m_rsc:p_no_acl(Id, is_published_date, Context) of
            undefined ->
                %% Let next observer decide
                undefined;
            true ->
                %% Resource is published: let next observer decide
                undefined;
            false ->
                %% Resource is unpublished
                case z_acl:is_allowed(update, Id, Context) of
                    true ->
                        %% User has update rights, so let next observer decide
                        undefined;
                    false ->
                        %% Deny viewing rights on unpublished resource
                        false
                end
        end;
    observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
        %% Fall through
        undefined.

In this observer, we return ``undefined`` in those cases where we do not
want to deny access. We don’t grant the access right away but give the next
observer the change to decide whether viewing is allowed (for instance, based on
the resource’s category and content group and the user’s group).

rsc_update
----------

An updated resource is about to be persisted. Observe this notification to
change the resource properties before they are persisted.

Arguments
.........

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
.......

Add a property before the resource is persisted::

    observe_rsc_update(#rsc_update{action = insert, id = Id}, {Modified, Props}, Context) ->
        %% Set an extra property
        {true, Props ++ [{extra_property, <<"special value!">>}].


rsc_update_done
---------------

An updated resource has just been persisted. Observe this notification to
execute follow-up actions for a resource update.

Arguments
.........

``#rsc_update``
    ``action``
        Either ``insert``, ``update`` or ``delete``.
    ``id``
        Id of the resource.
    ``pre_is_a``
        List of resource categories before the update.
    ``post_is_a``
        List of resource categories after the update.
    ``pre_props``
        List of properties before the update.
    ``post_props``
        List of properties after the update.

Context
    Site context

Return value
............

``ok``

Example
.......

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

