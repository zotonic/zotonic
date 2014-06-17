Release 0.10.1
==============

Released on 2014-06-17 20:27 by arjan.


Release highlights
------------------

New core features
.................

The properties of the resource model (m_rsc) can now be easily be
inspected with the 'print tag' and iterated over.


Updated modules
...............

mod_development
  Added dispatch debugging and explanation.
  Added checkbox to disable the api-service ``/api/development/recompile``

mod_admin_modules  
  Add "configure" button in the module manager for modules which contain a template called ``_admin_configure_module.tpl``.


New notification
................

Added the notification ``request_context``. This is a foldl with the `Context` and is 
called after the request’s query arguments are parsed using ``z_context:ensure_qs/1``.
It can be used to perform transformations or actions based on the query arguments.


Commits overview
----------------

Arjan Scherpenisse (11):

* build: Always use a downloaded rebar
* core: Add pivot columns for location lat / lng.
* core: Add zotonic_dispatch_path to z_context blacklist
* core: Fix comet/WS when site streamhost is undefined and redirect = false
* core: Let Travis build 0.10.x
* core: recode iso639 module to UTF-8
* doc: Clarify why Zotonic cannot directly use port 80
* mod_admin: Add option 'show_date' to 'latest resources' admin dashboard widget
* mod_admin: Correct vertical centering of connection dialog
* mod_admin_modules: Add "configure" button on module manager
* mod_base: Vertically center the z_dialog when it is opened


Arthur Clemens (19):

* doc: Add OS support, change name to Requirements
* doc: Improve docs sidebar layout a bit
* doc: Rename 'tutorials' to 'installation'
* doc: Tuning docs on services and mod_oauth
* doc: add example to filter on depiction
* doc: add link to manual dispatch
* doc: document restart command
* doc: elaborate conditional display of widget
* doc: fix 'button title' to 'button text'
* doc: show_media tag TLC
* doc: tweak highlight and see also colors
* doc: update Zotonic shell
* make new page button pass current category
* mod_admin: disable reindex button while indexing
* mod_admin: improve feedback
* mod_admin: live update of pivot queue count
* mod_editor_tinymce: introduce z_editor


Marc Worrell (24):

* core: added 'request_context' notification. Triggered after a request's query arguments are parsed.
* core: always set the user explicitly in the session. Don't copy the user from the current context.
* core: bump version to 0.10.0p1
* core: fix problem where z_db:insert can hang when running out of db connections.
* core: let the template {% print m.rsc[1] %} print all rescource properties. Same for {% for k,v in m.rsc[1] %}.%
* core: more binary/list changes (due to z_string changes)
* core: set default schema for sites without dbschema config.
* core: set the  option for Erlang 17.0 and later. Issue #764
* core: set the zotonic_dispatch_path to the path before an internal resource redirec.
* docs: add documentation for zotonic_dispatch_path_rewrite var
* docs: add more mod_development documentation.
* docs: add note about 0.10.0p1
* docs: added preliminary 0.10.1 release notes.
* mod_admin_frontend: fix for introduction of z_editor. Also sync the language tabs.
* mod_development: add access control to the dispatch rule debugger.
* mod_development: add checkbox to enable or disable the development/recompile api.
* mod_development: add human-readable form for rewrite_nomatch. Always show the final dispatch.
* mod_development: added request dispatching debug. This shows the steps of the dispatcher in matching a dispatch rule, including all bindings, rewrites etc.
* mod_development: added template lookup tool to check the template selection. Optionally add a resource castegory. Shows the selected template per user-agent class.
* mod_development: fix build of edoc.
* mod_development: fix problem where the dispatch-debugging interfered with ssl connection redirects.
* mod_development: show final dispatch after #dispatch_rewrite.
* mod_oembed: fix for z_string:to_name/1 now returning a binary.
* mod_survey: fix display of likert. Values were reversed from real values.


Mawuli Adzaku (1):

* mod_base: Add 'md5' filter to translate a string to an md5 hex value

