Copyrights
===========

This module implements an admin panel and templates to select and view the copyrights
attached to resources. It also adds attribution and source provenance to resources.

This modules uses two sets of rights statements:

 * https://creativecommons.org/licenses/#licenses-40
 * https://rightsstatements.org/page/1.0/?language=en

The properties managed are:

 * `rights`
 * `rights_attribution`
 * `rights_year`
 * `provenance_html` — optional, translatable rich text describing the source,
   collection, archive reference or publication.
 * `provenance_url` — optional, language-independent URL of the original source record.

Override the template `_copyright.tpl` to change the way the copyrights are displayed.

Provenance
----------

Attribution identifies who should receive credit. Provenance describes where the
material came from. For example, attribution can name a photographer, while
provenance identifies an archive collection and inventory number.

Edit provenance text using the resource's language tabs. Translation is optional;
normal resource language fallback applies when displaying the text. Keep archive
names and identifiers unchanged when translating descriptions. The source URL is
shared by all languages. HTML and URLs use the standard resource sanitization.

Display provenance explicitly with `{% include "_provenance.tpl" id=id %}`. This
renders nothing when both fields are empty. It is separate from the copyright
notice and does not use site-wide defaults. Existing site-specific source fields
are not migrated automatically.

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
