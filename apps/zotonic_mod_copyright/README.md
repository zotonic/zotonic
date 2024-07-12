Copyrights
===========

This modules implements an admin panel and templates to select and view the copyrights
attached to resources.  It also enables to add (legal) attribution to resources.

This modules uses two sets of rights statements:

 * https://creativecommons.org/licenses/#licenses-40
 * https://rightsstatements.org/page/1.0/?language=en

The properties manages are:

 * `rights`
 * `rights_attribution`
 * `rights_year`

Override the template `_copyright.tpl` to change the way the copyrights are displayed.

Defaults
--------

If no specific rights are defined then there are defaults:

 * `rights` - the config key `mod_copyright.rights` or otherwise `CR` (All rights reserved).
 * `attribution` - the config key `mod_copyright.attribution` or otherwise the site title.
 * `year` - the config key `mod_copyright.year` or otherwise the current year (in UTC).

Semantic Web
------------

The following mappings should be considered:

 * `rights`  schema:license / dcterms:license
 * `rights_attribution`  schema:copyrightHolder / dcterms:rightsHolder
