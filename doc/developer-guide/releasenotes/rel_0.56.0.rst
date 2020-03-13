.. _rel-0.56.0:

Release 0.56.0
==============

Welcome to Zotonic 0.56.0, released on ...

Possible breaking change:

We removed jquery-migrate from the standard template in
``modules/mod_base/templates/_js_include_jquery.tpl``.
If you need jquery migrate in your project then you can add
this template to your site and re-add ``"js/apps/jquery-migrate-1.4.1.min.js"``
to the include library files.

Main changes are:

 * Removed jquery-migrate from the standard jquery include

Commits since 0.55.0
--------------------
