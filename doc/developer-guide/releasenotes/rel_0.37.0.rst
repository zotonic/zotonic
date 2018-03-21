.. _rel-0.37.0:

Release 0.37.0
==============

Welcome to Zotonic 0.37.0, released on 5 March, 2018.

Main changes are:

* Added color argument to ``removebg``.
* Fixed static requests at root (``/``).

Commits since 0.36.0
--------------------

There were 5 commits since release 0.36.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (1):
    * mod_base: Fix controller_static_pages for requests at / (root)

Maas-Maarten Zeeman (1):
    * mod_component: Remove the polling workaround for missing onload functionality. It is slowing down chrome

Marc Worrell (3):
    * core: image option 'removebg' has now also an optional color to be removed
    * mod_menu: fix a problem where _menu_extra.tpl is not shown if the normal menu is empty.
    * core: Truncate texts for tsv indexes. Only index the first 30K
