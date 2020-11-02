.. _rel-0.58.0:

Release 0.58.0
==============

Welcome to Zotonic 0.58.0, released on November 2, 2020.

Main changes are:

 * ``{% image %}`` tags now have a width and height attribute, use the ``nowh`` option to
   suppress generation of the width and height attributes
 * Fix for a problem where an ACL deny rule could restrict too much
 * Support to store JSON in PostgreSQL
 * Fix a problem with Twitter/Facebook/LinkedIn authentication and Safari 14
 * Fix a problem where very large dates could give unexpected results
 * Better TLS cipher suite selection for SSL connections
 * Allow custom formatters to show an error on empty input values

To fix sites that break on the width/height attributes on image tags there is an option
added to the `zotonic.config` file: ``media_tag_nowh``

If the ``media_tag_nowh`` option is set to ``true`` then all ``{% image %}`` tags
have the ``nowh`` option added. Effectively suppressing the generation of width and height
attributes.

Note that this option is *not* available on the 1.x version and later of Zotonic.

If you have css like::

    img {
        max-width: 100%;
    }

Then add one line::

    img {
        max-width: 100%;
        height: auto;
    }


Commits since 0.57.0
--------------------

Arjan Scherpenisse (1):

 * mod_survey - option to hide survey form (#2432)

Maas-Maarten Zeeman (5):

 * Added encoding and decoding of json values (#2450)
 * Use sassc instead of ruby-sass (#2499)
 * Updated jquery to 3.5.1 (#2510)
 * Added an extra timeout before the page reload to give the browser more time to synchronize the cookie between tabs (#2525)
 * Bump mochiweb version (#2529)

Marc Worrell (16):

 * Upgrade mochiweb
 * Release 0.57.1 (#2436)
 * ACL: Fix a problem where a specific deny rule restricted other content to be found. Fixes #1964
 * Prevent a problem with calendar functions on large dates.
 * OEmbed vimeo endpoint moved to vimeo.com
 * Upgrade jsx to 3.0 and jsxrecord to 1.1. (#2484)
 * Set jsx to 3.0.0
 * Correct tag for jsx is v3.0.0
 * Correct NL translation
 * Fix a problem where changing language via javascript added an extra '?' to the url. Issue #2508 (#2509)
 * Allow error on empty input with Custom validator. Issue #2514 (#2515)
 * Fix a problem with dropbox file scanning for relative site directories.
 * Add dropbox upload dispatch rule
 * Add 0.x option to suppress wh attrs on image tags. Fixes #2527
 * Make hreflang link elements abs urls.
 * Fix a problem with OAuth flow on Safari 14. Issue #2536 (#2537)

Ruben Lie King (1):

 * Log errors on resource update (#2523)
