Release 0.10.1 (unreleased)
===========================

Updated modules
---------------

mod_development
  Added dispatch debugging and explanation.
  Added checkbox to disable the api-service ``/api/development/recompile``

New notification
----------------

Added the notification ``request_context``. This is a foldl with the `Context` and is 
called after the request’s query arguments are parsed using ``z_context:ensure_qs/1``.
It can be used to perform transformations or actions based on the query arguments.
