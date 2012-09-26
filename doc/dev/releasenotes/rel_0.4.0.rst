Release 0.4.0
=============

Released on 2010-04-19.


New modules
-----------

mod_pubsub
   Enables resource sharing over XMPP's PubSub; share content between
   sites and get realtime updates when content changes. See:
   http://scherpenisse.net/id/644

mod_search_solr
   Added a module which plugs into Zotonic's search system to support
   Solr (http://lucene.apache.org/solr/). Using Solr enables quick
   fulltext searching and facetting.


New features
------------

Default site improvements
   The default site of a vanilla Zotonic install has been improved
   with nicer graphics, cleaner typography, a "contact form" example
   and styles for the Twitter module.

"More results" scomp
   A twitter/facebook style ajaxified "read more" pager, which is a
   button which will fetch more results for the current search
   question inline on the same page.

Windows support
   Initial support for building and running Zotonic on the Windows
   platform.

Database schema support
   Multiple sites running inside one Postgres database is now possible
   thanks to Postgres' support for multiple table namespaces
   (schema's)

Template expressions
   It is now possible to use full boolean and arithmetic expressions
   in the ErlyDTL templates.

Other features:

* Webserver IPv6 support 
* Yandex.Video support in mod_video_embed module (#52)
* PID-file for zotonic (#74)
* Support for HTML5 audio/video tags in TinyMCE editor (#75)
* Newer TinyMCE 3.3.2 release from upstream (#69)
* Newer Mochiweb r153 release from upstream


  Bugfixes
--------

* page_path controller should not redirect to the default_page_url (#6)
* Get the name of the current dispatch rule (#21)
* zotonic fails after postgresql restart (#49)
* Unreliable pivot? (#50)
* Module manager should feedback when module cannot be started. (#51)
* Do not depend on the 'default' site (#59)
* i18n of scomp_pager (#62)
* Buttons and "Logoff" link problems in Chrome (#63)
* Comment form breaks on new default site (#64)
* Getting an unknown_rsc error on startup (#66)
* Zotonic fails to (re)start if an existing admin panel is open with browser supporting WebSockets (#70)
* Can't save location without e-mail (#71)
* Improve the default styles to include list bullets/numbers (#72)
* Twitter module cannot be enabled (#76)
