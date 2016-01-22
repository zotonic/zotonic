.. _rel-0.13.2:

Release 0.13.2
==============

Welcome Zotonic 0.13.2, released on August 11, 2015.

This is a maintenance release of Zotonic 0.13

The main change is that this version compiles correctly on Erlang 18.


Commits since 0.13.1
--------------------

There were 15 commits since release 0.13.1.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Marc Worrell (15):

*  docs: fix ref in 0.13.1 release notes.
*  mod_oauth: fix user_id initialization on direct app creation
*  mod_oauth: use the abs_url of the request path for checking the signature. This fixes an issue where OAuth access to https sites is denied.
*  mod_content_groups: also add the content group as related data to the pivot.
*  mod_base/mod_admin: add optional redirect to the set website, using 'is_website_redirect'. Allow overruling the controller's 'is_canonical' flag with 'is_page_path_multiple'. Add documentation for rsc properties 'is_dependent', 'content_group_id', 'is_page_path_multiple' and 'is_webite_redirect'.
*  New z_stdlib
*  core: allow max 5 smtp connections per relay, set max parallel smtp senders to 100. Fixes #993
*  mod_survey: open survey results from mod_admin_frontend in new window.
*  mod_base: hide any files and directories whose name starts with a '.' Fixes #991
*  core: fix compilation on Erlang 18.0. Fixes #979
*  core: add erlang 18 as travis target.
*  Locked new deps.
*  Preliminary 0.13.2 release and release notes.
*  New z_stdlib
*  mod_content_groups: on pivot add the complete path for the content-group as related ids.
