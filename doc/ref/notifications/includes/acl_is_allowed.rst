.. include:: includes/meta-acl_is_allowed.rst

Example
"""""""

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
