Release 0.12.3
==============

Released on 2014-12-22 10:30 by mworrell.

Major changes are:

* Fix of the twerl git url in rebar.config.lock
* New z_stdlib, fixing issues with url metadata extraction
* Sanitization of oembed content
* Addition of Font Awesome 4
* Extended Zotonic logo font


The following commits were done
-------------------------------

Arthur Clemens (20):

* admin: general layout fixes
* admin: use zotonic icons
* doc: update logo and link style
* fix gitignore
* mod_admin: remove unused files
* mod_admin: update with mod_base changes
* mod_artwork: add font awesome 4
* mod_artwork: add zotonic logos
* mod_artwork: rename logo folder to "zotonic"
* mod_base: allow other libs to import z.icons.less
* mod_base: bring back (predictable) circle icons
* mod_base: extend logo font to include general icons for Zotonic modules
* mod_base: make extra tag more compatible
* mod_base: replace some icons with FA versions, add button styles
* mod_base: simplify icon creation; add extending FA icons
* mod_base: update favicon
* Remove unused files
* zotonic_status: add less files
* zotonic_status: make responsive, update appearance
* zotonic_status: notification tweaks


Marc Worrell (8):

* core: add instagram js and urls to whitelist.
* core: lock new z_stdlib library. Fix twerl git url. Fixes #895
* mod_admin: always show media content, independent if size was defined.
* mod_oembed/mod_twitter: prefer our own 'website' extraction above oembed links. Pass the tweet url in the import_resource notification
* mod_oembed: add missing fallback _oembed_embeddable.tpl
* mod_oembed: sanitize received oembed code html and texts.
* mod_survey: fix problem where displaying the results did not work due to move sanitization functions.
* mod_twitter: fix import of tweets with special-html chars, was double escaped in title.
