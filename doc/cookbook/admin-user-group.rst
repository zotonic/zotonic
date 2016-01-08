Automatically add new users to a user group
============================================

Why
---

When you create a person, you usually need to add it to a user group as well.
You may want to automate this, in particular if you need to differentiate the
user group based on the person’s category.

.. note::

    If you merely want to change the default user group for *all* users, you can
    do so through a :ref:`configuration parameter <mod-signup-new-users-content-group>`
    and you don’t need the solution below.

Solution
--------

You can set the user group by adding a ``hasusergroup`` property to the
resource. In your site’s main ``.erl`` file, add a ``rsc_update`` observer::

    -export([
        observe_rsc_update/3
    ]).

    observe_rsc_update(#rsc_update{action = insert, id = Id}, {Modified, Props} = Acc, Context) ->
        %% Where 'vip' is a subcategory of 'person'
        case m_rsc:is_a(Id, vip, Context) of
            false ->
                %% Do nothing
                Acc;
            true ->
                %% Add hasusergroup property
                {true, Props ++ [{hasusergroup, m_rsc:rid(acl_user_group_vips, Context)}]}
        end;
    observe_rsc_update(#rsc_update{}, Acc, _Context) ->
        %% Fall through
        Acc.
