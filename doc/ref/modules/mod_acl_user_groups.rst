
.. include:: meta-mod_acl_user_groups.rst

This module adds rule-based access control.

* All resources (pages) are put into a content group.
* All users are member of zero or more user groups.
* Content groups are arranged in a hierarchy.
* User groups are arranged in a hierarchy.

ACL rules are defined between user groups and content groups. Each single rule
gives some user group one or more permissions on some content group.

Managing user groups
--------------------

By default, Zotonic has four user groups:

* anonymous (anonymous visitors of the website)
* members (logged in users of the website)
* editors (content editors)
* managers (manage users)

These user groups are arranged in a hierarchy, so that each group has the
permissions its parent plus more. So, the permissions of users in the members
group include those of anonymous users; and editors have the permissions of both
anonymous users and members plus more.

To add or remove user groups, go to *Auth > User groups* in the admin.

.. _collaboration groups:

Collaboration groups
^^^^^^^^^^^^^^^^^^^^

Collaboration groups are a special type of user groups. They are most useful
when you have groups of users that together collaborate on content. All content
belongs to the group. Each collaboration group has one or more managers. So if
you have groups of students working together and being supervised by teachers,
you can define them as collaboration groups with the teachers as managers.

.. _content groups:

Managing content groups
-----------------------

By default, Zotonic has two content groups:

* default (all site content)
* system content (categories, predicates and user groups)

To add or remove content groups, go to *Structure > Content groups* in the
admin. Just like user groups, content groups are ordered in a hierarchy. So the
permissions that apply to the parent content group also apply to all of its
children.

Defining access rules
---------------------

You can add ACL rules in two ways:

1. in the admin web interface at ``http://yoursite/admin/acl/rules``
2. in your site’s or module’s code; see :ref:`managed rules` below.

Let’s start by defining rules in the web interface.

.. _content-acl:

Content access rules
^^^^^^^^^^^^^^^^^^^^

Each content access control rule grants some user group one or more permissions
on some content group. So, for each rule you must specify the following
properties:

* User group
* Content group
* Resource category (or ‘all categories’)
* Permissions: ‘view’, ‘insert’, ‘update’, ‘delete’, ‘link’, ‘manage own’.

If you wish to narrow down the rule, you can select a single resource category
it applies to. The default is ‘all categories’.

The content group dropdown contains:

* all your :ref:`content groups`
* all your :ref:`collaboration groups`

The permissions include simple resource permissions that determine whether users
in the group are allowed to view, insert, update or delete resources. The
‘link’ permission is about creating outgoing edges from resources in the
content group to other resources.

Some rules may be greyed out and have a note saying ‘This rule is managed by
module …’. These are :ref:`managed rules` that you cannot edit in the web
interface.

Collaboration group rules
~~~~~~~~~~~~~~~~~~~~~~~~~

Collaboration rules are special content access rules that apply to content in
:ref:`collaboration groups <collaboration groups>` only. Each rule applies to
all collaboration groups.


Access control on properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some private sensitive resource properties are protected by the ACL rules.
The *privacy* property defines who can see these properties.

The default privacy for category *person* is *collaboration group members*.
For other categories the default is *public*.

The privacy property can have the following values:

 * 0 public
 * 10 members
 * 20 member of same user group (except default members group)
 * 30 collaboration group members
 * 40 collaboration group managers
 * 50 private

Increments are 10 so that more refined options can be added by custom modules.

The protected properties are:

 * email
 * phone
 * phone_mobile
 * phone_alt
 * address_street_1
 * address_street_2
 * address_postcode
 * address_city
 * date_start
 * date_end
 * location_lat
 * location_lng
 * pivot_location_lat
 * pivot_location_lng
 * pivot_geocode
 * pivot_geocode_qhash


.. _module-acl:

Module access rules
^^^^^^^^^^^^^^^^^^^

Each module access rule grants some user group `use` permissions on some
module. In the admin, go to *Auth > Access control rules > Modules* tab to
edit them.

Deny rules
^^^^^^^^^^

By default, rules grant some permissions. But sometimes you want to deny some
permissions that are granted by another rule. For instance, if you have a rule
that allows anonymous users to view all content groups, but you have a special
content group ‘Top secret’ that you want to hide from anonymous users, add a
rule to deny access:

+------+----------------+---------------+----------+-------------+
| Deny | ACL user group | Content group | Category | Permissions |
+======+================+===============+==========+=============+
| √    | Anonymous      | Top secret    | All      | √ View      |
+------+----------------+---------------+----------+-------------+

.. _publishing rules:

Publishing rules
----------------

When you’re editing rules, they are not effective immediately: you have to
publish them first. Click the ‘Publish’ button to do so.

You can test out your rules before publishing them by clicking the
‘Try rules…’ button.

.. _managed rules:

Managed rules
-------------

Above you’ve seen how you can add rules through the web interface. Using
:ref:`module versioning <guide-modules-versioning>`, you can also write rules
in your code. These rules are called ‘managed rules’ because they are defined
in the code of modules, including your own
:ref:`site module <guide-site-anatomy>`.

While editing a simple set of ACL rules in the web interface is easier for end
users, developers may prefer to manage more complex rules in code. Managed rules
have two important advantages:

* they are equal between all environments (such as development, acceptance and
  production)
* when developing and deploying new features, ACL rules and code often belong
  together. By defining the rules in your code, you can commit and store them
  along with the feature code.

If you haven’t yet done so, set up
:ref:`module versioning <guide-modules-versioning>` in ``yoursite.erl`` or
``mod_your_module.erl``. Then, in the ``manage_schema/2`` function, add an
``acl_rules`` section under the ``data`` property in the ``#datamodel{}``
record::

    %% yoursite.erl
    -module(yoursite).
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
                    %% A resource ACL rule is defined as {rsc, Properties}
                    {rsc, [
                        {acl_user_group_id, acl_user_group_members},
                        {actions, [view, link]},
                        {is_owner, true},
                        {category_id, person}
                    ]},

                    %% A module rule is defined as {module, Properties}
                    {module, [
                            {acl_user_group_id, acl_user_group_editors},
                            {actions, [use]},
                            {module, mod_ginger_base}
                        ]
                    },

                    %% A collaboration group rule is defined as {collab, Properties}
                    {collab, [
                        {is_owner, true},
                        {actions, [view, insert, update, link]},
                        {category_id, text]
                    ]}
                ]
            ]
        }.

    manage_schema({upgrade, 2}, Context) ->
        %% code to upgrade from version 1 to 2
        ok;

Compile the code and restart your module to load the managed rules. They will be
added and immediately :ref:`published <publishing rules>`.

The set of rules defined in ``manage_schema/2`` is *declarative* and *complete*.
That is to say, you declare the full set of rules that you wish to define. Any
changes or deletions that you make to the rules in your code, will propagate to
the site’s rules. To protect the set’s completeness, managed rules cannot be
altered in the web interface.

Exporting and importing rules
-----------------------------

To back up your rules, go to *Auth > Access control rules* and click the
‘Export edit rules’ button. The backup will include the full hierarchies of
all user and content groups.

You can import a previous backup by clicking the ‘Import edit rules…’ button.
