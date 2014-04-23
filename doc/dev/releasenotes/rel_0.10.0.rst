Release 0.10.0
==============

Welcome Zotonic 0.10.0, released on April 18, 2014. These notes list
the most important changes for this new feature release.


New core features
------------------

The minimum required Erlang version is now release R15B03. Zotonic is
always supporting up to major 2 versions behind the current stable
Erlang release.
 
We now use the de-factor Erlang package manager rebar for the
management of the subprojects. 

Websocket / comet (re)connection handling have been greatly
improved. A state machine now manages the process on the server and
keeps the connection running also in the long run.

The Google Summer of Code-sponsored student Mawuli Adzaku provided
excellent work on the Zotonic CLI for management (search,
installation, ...) of external Zotonic modules such as those from
http://modules.zotonic.com/

The media handling subsystem was heavily modified in this release. It
is now much easier to add custom hooks to do custom media processing
when a media file has been uploaded. The following notifications were
added as extension points:

 * ``#media_identify_extension{}`` to make custom mappings from a mime-type to an extension
 * ``#media_identify_file{}`` to override media file identification
 * ``#media_update_done{}`` when the media metadata has been stored in the database
  
For SEO purposes, a new table `rsc_page_path_log` was added, that tracks old page_path
names from :term:`resources <resource>` so that old urls keep working
when the page path changes. Old URLs will be redirected to the new
location instead.

Handling legacy username/password authentication can now be done by
leveraging the new ``#identity_password_match{}`` notification.


New and updated modules
-----------------------

mod_seo
  The two modules mod_seo and mod_seo_google have been merged into a
  single module. Support for Bing webmaster verification has been
  added.

mod_artwork
  This module now ships with the font-awesome icon set.

mod_admin
  A much requested feature to set the "center of gravity" for images
  has been added in the admin. The 'crop' filter takes this center
  into account.

mod_admin_frontend
  This new module will provide admin-alike editing of resources,
  menu-trees and collections for non-admin users in the frontend of
  the website, by the re-use of admin templates in the frontend
  site. This module was backported to the 0.9 series.

mod_admin_stats
  The admin stats module adds a new admin page presenting a live view
  of system statistics for things like number of requests, request
  processing times, database and cache queries etc.

mod_custom_redirect
  This module provides hooks to redirect unknown request paths or
  hosts to other locations. It adds a page to the admin to define and
  maintian these redirects. This module was backported to the 0.9 series.
     
mod_video
  A new module for video conversion and playback. It uses theffmpeg
  tool to convert uploaded movies to h264 and generate a poster
  image. Included are default templates for movie playback.
 
mod_mqtt
  Interfaces to MQTT publish/subscribe topics. Adds publish/subscribe
  between the browser, server and modules. Includes access control and
  topic mapping for external MQTT applications.


Template system improvements
----------------------------

The template include system was made more powerful by adding options
to force the runtime include of a template and to make the include
optional (e.g. not crashing when a template is not found).

The option was added (to :ref:`mod_development`) to generate HTML
comments with the paths to the included files embedded for easy
debugging.

Besides this, the erlydtl_runtime can now directly lookup values in
mochiweb JSON structures, making template syntax more straighforward.
  

New template filters
-------------------- 

trim
  Removes whitespace at the start and end of a string.

pickle
  Pickle an Erlang value so that it can be safely submitted with a form.

tokens
  Returns a list of tokens from input string, separated by the characters in the filter argument.

filter
  Filters a list on the value of a property.

sort
  Sorts lists in various ways

if_undefined
  Tests whether a value is undefined, returning the given argument.

menu_is_visible
  Filters a list of menu items on visibility.

menu_rsc
  Get a "flat" of menu parents  



Translations
------------

Translations were added and updated for Chinese (ZH), Russian,
Turkish, Dutch, Polish, Portugese, French and German.


Contributors
------------

The following people were involved in this release:

Alexander Stein, Andreas Stenius, Arjan Scherpenisse, Arthur Clemens,
Bryan Stenson, Carlo Pires, Cillian de Róiste, Feather Andelf,
furiston, Grzegorz Junka, Ilyas Gasanov, Jarimatti Valkonen,
Maas-Maarten Zeeman, Marc Worrell, Mawuli Adzaku, Mgpld, Piotr Nosek,
Simon Smithies and Steffen Hanikel.
