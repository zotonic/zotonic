.. _rel-0.25.0:

Release 0.25.0
==============

Welcome to Zotonic 0.25.0, released on 6 February, 2017.

Main changes are:

* Added ``embedded_media`` filter (:issue:`1591`).
* Added Atom support to mod_export (:issue:`1394`).
* Added ``name`` search query argument (:issue:`1574`).
* Fixed image crop defined in mediaclass (:issue:`919`).
* Fixed adding connection to page block (:issue:`1561`).
* Fixed text blocks not considered by ``without_embedded_media`` filter
  (:issue:`1566`).
* Fixed unique name validation for empty names (:issue:`1579`).
* Fixed filewatcher reacting to changes in log files (:issue:`1588`).
* Fixed pivot index names not unique (:issue:`1598`).

Commits since 0.24.0
--------------------

There were since 33 commits since release 0.24.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arthur Clemens (1):
    * Add clarification in template

David de Boer (6):
    * doc: Add custom controllers cookbook chapter (#1565)
    * doc: Fix postback validator documentation (#1567)
    * docker: Build Erlang from source (fix #1590)
    * core: Raise timeout when starting site for sitetest
    * doc: Fix embedded_media docs
    * core: Make pivot index name unique (#1598)

Maas-Maarten Zeeman (1):
    * core: Accept push queue names as binaries. Fixes #1575 (#1576)

Marc Worrell (24):
    * mod_search: use publication_start as a sub sort for matching, prefer newer content.
    * core: evaluate 'and'/'or' as 'andalso' and 'orelse'. Fixes #1561
    * mod_base: fix a problem in show_media where utf8 characters were wrongly displayed. Fixes #1572
    * mod_search: add a query term 'name' for searching on unqiue names. Fixes #1574
    * mod_export: add export in application/atom+xml format. Fixes #1546  Fixes #1394
    * Lock new z_stdlib
    * core: let filewatcher ignore changes to log files. Fixes #1588
    * core: also ignore changes to files in the mnesia directory
    * mod_base: in ubf.js use Object.keys() to fetch the keys of an object. This fixes an issue where injected object.prototype functions were serialized as well.
    * docs: fix build problem with 'less' code block by using 'text' instead
    * mod_base: let filter without_embedded_media also consider text blocks. Add filter embedded_media. Fixes #1566
    * core: rsc names with only a '_' are considered 'undefined'. Fixed #1579
    * mod_base: let the unique name validation first use 'z_string:to_name/1'. Consider '_' as an invalid unique name. Issue #1579
    * mod_base: allow empty unique name.
    * core: use crop_center if crop is defined in the mediaclass. Fixes #919
    * mod_admin: fix a problem with media preview for unknown resources.
    * mod_base: fix ubf decode for plist/map types.
    * mod_admin: change positioning of images in connection lists. Fixes #1570
    * core: check crop_center syntax on rsc update.
    * core: add email DNS Whitelisting support
    * Lock new z_stdlib
    * core: fix a string/binary conversion issue with the new z_stdlib
    * New z_stdlib
    * core: fix a problem where next/prev_day could result in a non-existant date. Fixes #1596

row-b (1):
    * Minor typo in syntax fixed (#1569)
