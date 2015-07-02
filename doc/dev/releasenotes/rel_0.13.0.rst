.. _rel-0.13.0:

Release 0.13.0
==============

Welcome Zotonic 0.13.0, released on July 3, 2015.

Major changes are:

 * New ACL module: mod_acl_user_groups (see below)
 * New module: mod_content_groups
 * New model: m_hierarchy, for category, content_group and user_group hierarchies
 * Category renumbering is now a lot faster and more incremental
 * Addition of Material Design art work
 * New module: mod_email_status
 * New module: mod_component
 * DNSBL checks for incoming emails
 * More strict sanitization of content
 * Social integration with Twitter, Facebook, LinkedIn, and Instagram
 * *is_dependent* Resources

This release also incorporates all fixes in the 0.12.x branch.


Documentation
-------------

Unfortunately we still miss some documentation for the new modules.
This will be corrected as soon as possible.


New Access Control module
-------------------------

The new *mod_acl_user_groups* module adds rule based access control.

 * All resources (pages) are assigned a content group.
 * All users are member of zero or more user groups.
 * Content groups are arranged in an hierarchy
 * User groups are arranged in an hierarchy

Rules are defined between the user groups and the content groups.
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


Categories and m_hierarchy
--------------------------

The category hierarchy tables have been replaced by *m_hierarchy*.
This model defines named hierarchies of resources (pages).

If the categories are changed then the system needs to update the
*pivot_category_nr* field of all resources. With the introduction 
of *m_hierarchy* this renumbering is much more efficient and will 
only affect a minimal number of resources.

The *m_hierarchy* module is also used for the content- and user group
hierarchies, as used by the new *mod_acl_user_groups* module.


Email status
------------

The new module *mod_email_status* tracks for all outgoing email addresses:

 * If emails are successfully sent
 * Number of emails sent
 * Number of errors
 * Number of bounces
 * Latest error message

With this it will be much easier to get feedback on all outgoing email.


DNSBL checks
------------

All incoming SMTP connections are now checked against two DNS block lists:

 * ``zen.spamhaus.org`` (http://www.spamhaus.org/zen/)
 * ``dnsbl.sorbs.net`` (http://dnsbl.sorbs.net/general/using.shtml)

The list of DNSBL servers checked can be configured using the ``smtp_dnsbl`` 
key in the ``zotonic.config`` file.


mod_component
-------------

This is an experimental module. It will be the basis of a new method of
loading *components* in HTML pages. Components consist of HTML, javascript,
and css. These parts can be dynamically loaded. Layout can/will be done
using *mithril.js*.

This module will undergo significant changes, so use at your own risk.
It is included to support some parties that use this in production.


Sanitization of content
-----------------------

``iframe`` Elements and CSS are now sanitized. The *iframes* are sanitized
using a whitelist of servers. Use the ``#sanitize_embed_url{}`` notification
to add or remove servers from the whitelist. The current whitelist can be
seen at the bottom of the ``src/support/z_sanitize.erl`` file.

*Object* tags referring to services like *youtube* are replaced with *iframe*
tags.

This sanitization is also done on all *embed* codes of media items.

The CSS parser is strict and only allows well formed CSS. It also strips
CSS that is known to be (potentially) *dangerous*:

 * loading external content
 * ``position: screen``
 * some classes that are used internally in Zotonic
 * and some more

Check ``deps/z_stdlib/src/z_css.erl`` for the actual sanitizer code.


Social integration
------------------

The integration with social sites is completely redone. It is now possible to
login (and signup) with Facebook, Twitter, LinkedIn, and Instagram.

It is also possible to import content from Twitter and Instagram. For this
it is possible to define tags and/or users to follow. All matching content
is imported into special categories. An optional image or embed code is also
imported and added *as part of* the imported resource (so not as a separate
depiction).


*is_dependent* Resources
------------------------

A new flag is added to all resources: *is_dependent*

If this flag is set, and a connection to the resource is deleted, then Zotonic will
check if there are any other *incoming* connections to the resource. If not then
the resource will automatically be deleted.

This makes it possible to have resources (images, documents) that can only exist
in the context of another resource. If that referring resource is deleted then the
depending resources are deleted as well.






