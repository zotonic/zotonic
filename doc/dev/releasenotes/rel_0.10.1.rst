Release 0.10.1 (unreleased)
===========================

New core features
-----------------

The properties of the resource model (m_rsc) can now be easily be
inspected with the 'print tag' and iterated over.

Updated modules
---------------

mod_development
  Added dispatch debugging and explanation.
  Added checkbox to disable the api-service ``/api/development/recompile``

mod_admin_modules  
  Add "configure" button in the module manager for modules which contain a template called ``_admin_configure_module.tpl``.


New notification
----------------

Added the notification ``request_context``. This is a foldl with the `Context` and is 
called after the request’s query arguments are parsed using ``z_context:ensure_qs/1``.
It can be used to perform transformations or actions based on the query arguments.
