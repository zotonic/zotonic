.. _rel-0.47.0:

Release 0.47.0
==============

Welcome to Zotonic 0.47.0, released on 4 March, 2019.

Main changes are:

 * Fix for a problem where many websockets could be opened
 * Fix for a probem duplicating resources with custom slugs
 * Fix for a potential xss issue in the mod_admin
 * Unpublished resources are now created without an publication date
 * New option per predicate to insert new edges as the first edge
 * New option to force users to re-agree with updated terms or privacy documents


Commits since 0.46.0
--------------------

David de Boer (3):

 * doc: Explain anonymous context in task queue callback (#2007)
 * core: Default publication_start to null (#2008)
 * core: Fix publication_start default (#2015)

Dorien (1):

 * Fix #2009: Add lists to plugins to support lists in editor (#2010)

Maas-Maarten Zeeman (2):

 * core: Fix the accidental creation of a global variable
 * core: Prevent too many WebSockets (#2014)

Marc Worrell (11):

 * mod_editor_tinymce: remove branding.
 * mod_admin: fix a problem with escaping values in admin_button_dropdown template. (#2006)
 * mod_admin: rename 'selected_value' to 'selected_qvalue' to make clear it is an unescaped query arg.
 * mod_seo: allow site config settings overrule the seo.noindex config.
 * core: dump sql queries as string in errors.
 * core: Edge insert top (#2011)
 * mod_authentication: Add T&C re-agreement and restructure logon errors (#2012)

loetie (1):

 * When duplicate reset custom_slug boolean (#2005)
