
.. include:: meta-mod_acl_user_groups.rst

This module adds rule based access control.

 * All resources (pages) are assigned a content group.
 * All users are member of zero or more user groups.
 * Content groups are arranged in an hierarchy
 * User groups are arranged in an hierarchy

ACL rules are defined between the user groups and the content groups.
Per rule the following properties can be set:

 * User group
 * Content group
 * Category (or *all categories*)
 * Privileges: view, edit, link, delete
 * Deny flag (for a negative rule)

The collection of rules has an *edit* and *publish* version.
The *edit* version can be tested with a special url.
If all is accepted, then the *edit* version van be published.

The *edit* version can also be exported and imported.
This includes the full hierarchies of all user- and content groups.

Defining ACL rules
------------------

You can add ACL rules in two ways:

1. in the admin web interface at ``http://yoursite/admin/acl/rules``
2. in your site’s or module’s code using
   :ref:`module versioning <guide-modules-versioning>`.

While editing a simple set of ACL rules in the web interface is easier for end
users, developers may prefer to manage more complex rules in code. Code-based
rules have two important advantages. First, they are equal between all
environments (such as development, acceptance and production). Secondly, when
developing and deploying new features, ACL rules and code often belong together.
By defining the rules in your code, you can commit and store them along with the
feature code.

Managing ACL rules in code
^^^^^^^^^^^^^^^^^^^^^^^^^^

If you haven’t yet done so, set up
:ref:`module versioning <guide-modules-versioning>` in ``your_site.erl`` or
``mod_your_module.erl``. Then, in the ``manage_schema/2`` function, add an
``acl_rules`` section under the ``data`` property in the ``#datamodel{}``
record::

    -module(mod_your_module).
    -mod_schema(1).

    -export([
        manage_schema/2
    ]).

    %% .... more code here...

    manage_schema(install, Context) ->
        #datamodel{
            %% your resources...
            data = [
                {acl_rules, [
                    %% A resource ACL rule is defined as {rsc, Properties}.
                    {rsc, [
                        {acl_user_group_id, acl_user_group_members},
                        {actions, [view, link]},
                        {is_owner, true},
                        {category_id, person}
                    ]},

                    %% A module rule is defined as {module, Properties}
                    {module,
                        [
                            {acl_user_group_id, acl_user_group_editors},
                            {actions, use},
                            {module, mod_ginger_base}
                        ]
                    }
                ]
            ]
        }.

    manage_schema({upgrade, 2}, Context) ->
        %% code to upgrade from 1 to 2
        ok;

Compile the code and restart your module to load the managed rules.

The set of rules defined in ``manage_schema/2`` is *declarative* and *complete*.
That is to say, you declare the full set of rules that you wish to define. Any
changes or deletions that you make to the rules in your code, will propagate to
the site’s rules. The protect the set’s completeness, managed rules cannot be
altered in the web interface.
