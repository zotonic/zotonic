.. _rel-0.36.0:

Release 0.36.0
==============

Welcome to Zotonic 0.36.0, released on 5 February, 2018.

Main changes are:

* Added support for BCE dates and era element (:issue:`1869`, :issue:`1865`).
* Added compatibility with Erlang/OTP 20 (:issue:`1864`).
* Fixed parse error in search queries (:issue:`1854`).
* Fixed empty image alt tag (:issue:`1868`).

Commits since 0.35.0
--------------------

There were 11 commits since release 0.35.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (2):
    * deps: Fix BC date handling (#1865)
    * docs: Incorporate changes from z_stdlib to improve date handling (#1869)

Maas-Maarten Zeeman (1):
    * mod_base: Added filter_round from master to 0.x branch

Marc Worrell (8):
    * mod_oauth: support OAuth signature check on POST to controller_api. (#1862)
    * Make 0.x compatible with OTP20 (#1864)
    * mod_atom_feed: fix a problem with search term selection for /feed/search
    * mod_base: fix the static template lookup.
    * docs: add docs for 'round' filter.
    * Fix a problem with parsing custom filters. Fixes #1854 (#1866)
    * core: fix a problem where an empty 'alt' attribute was suppressed. Fixes #1868
    * core: image option 'removebg' has now also an optional color to be removed
