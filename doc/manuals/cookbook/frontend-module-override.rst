Modifying an existing module
============================

Safely overriding core modules to make them do what you need or want.

Why
---

Sometimes a module presents things on screen in a way you don't like.
It might be that a certain piece is missing or the order of the
elements breaks something you are trying to do.  This guide presents
some techniques for working around this.

Assumptions
-----------

Readers are assumed to be comfortable editing templates and
identifying the source of content by searching the Zotonic codebase.

How
---

If you want to just change the look and feel, you should be able to
get most of the way with CSS: create a CSS file in your site, and
override the styles you don't like.

Failing that, you should try overriding templates: Copy the module's
templates into your own site/module, keeping the file name and
relative directory intact.

Make sure that the `priority` of your site is higher than the original
module! To view this, check the `prio` field in the module overview in
the admin, this should be a lower number. e.g. 1 has higher prio
than 500.

Changing the Logic of a Module
..............................

In extreme cases, you can copy the module into the modules directory
of your site and rename it.  Make sure to change the module attributes
within the Erlang modules along with renaming the files themselves.

A case for this would be an outdated core module that no longer works
with an API or protocol due to external changes.  For example, if
Twitter dropped their current streaming interface (unlikely) then you
might need to copy and modify mod_twitter in order to bridge the gap
until you submit a patch and its gets accepted.
