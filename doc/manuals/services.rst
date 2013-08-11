.. _manual-services:

API Services
============

Services provide a generalized way to create API
calls. These calls automatically use the authentication mechanism
(session id or OAuth) to perform access checks.

Serving services
----------------

In a default Zotonic install, there is a single URL namespace under which all API services can be accessed. :ref:`controller-api` by default intercepts all URLs according to the
following patterns::

  /api/:module/:method
  /api/:module``


On these URL, a lookup is done to the Zotonic module named ``mod_:module``
in its ``services/`` directory, for a Erlang file called
`service_:module_:method.erl`. If method is left empty (at the
``/api/:module`` URL), the method name equals the module name.

So for example the following lookups result in the following service handlers:

=================  ==========   ========   ====================================
URL                Module       Method     Found service .erl file
=================  ==========   ========   ====================================
/api/base/export   mod_base     export     mod_base/services/service_base_export.erl
/api/base/info     mod_base     info       mod_base/services/service_base_info.erl
/api/search        mod_search   search     mod_search/services/service_search_search.erl
=================  ==========   ========   ====================================

For creating services at alternative URLs, see
:ref:`controller-api-nonstandard-url` in the :ref:`controller-api`
documentation.


Service naming in detail
........................

As stated above, a service module is defined like this::

  service_<modulename>_<processname>.erl
    
And is then reachable on the URL
``http://<hostname>/api/<module_name>/<process_name>``. Important: the
module that the service is in needs to be has to be activated.

Say you have a module named `mod_something`, and it is activated and
you want a service to return stats.  Your directory would look like
this::

  mod_something/
    mod_something.erl
    services/
      service_something_stats.erl

And the url for this service ``http://<site_addr>/api/something/stats``

The key is that an activated module (minus the ``mod_`` prefix if you use
them!) should be part of the service name. Zotonic parses the service
modules filename to identify what module a service relates to and what
process should be called.  It checks to make sure that module is
activated and it also uses that same information when matching a
service url. So, reversly, ``service_something_stats.erl`` is served by
``http://<hostname>/api/something/stats``.


.. highlight:: erlang

Service metadata
----------------

Like stated, any service is a regular :term:`Erlang module`. There are
however a few extra attributes for use in the service which describe
it more. Firstly, there is ``svc_title``::

  -svc_title("Retrieve uptime statistics.").

The title of a service should be a human-readable, one-line
description of what the services does. This title is used in the OAuth
authentication dialog: when authorizing an application, the titles of
the services that it wants to access are listed, for the authorizing
user's consideration.

Secondary there is ``svc_needauth``::

  -svc_needauth(false).

This is a boolean value which tells the system whether or not a user
needs to be authorized in order to use the service.

If authentication is needed for a service, a service can only be
accessed either by using the session cookie or by using an authorized
OAuth (1.0a) token.


Creating a GET service
----------------------

By implementing the ``process_get/2`` function in your service module,
it indicates that it is able to handle GET requests.  A full example
of a services which handles a GET request is listed below::

  -module(service_something_stats).
  -author("Arjan Scherpenisse <arjan@scherpenisse.net>").

  -svc_title("Retrieve uptime statistics of the system.").
  -svc_needauth(true).

  -export([process_get/2]).

  -include_lib("zotonic.hrl").

  process_get(_ReqData, _Context) ->
      Stats = [{count, 12310},
               {uptime, 399}],
      z_convert:to_json(Stats).

This module could be called ``service_something_stats.erl`` and then
gets served at ``/api/something/stats``. Its output is a JSON object
containing a `count` and an `uptime` field, containing some values.

Of course, you would write real code there which retrieves actual stats. If your module `something` contains the function ``stats_data/1``, call it from the process function like this::

  process_get(_ReqData, Context) ->
      Stats = mod_something:stats_data(Context),
      z_convert:to_json(Stats).
      
Creating a POST service
-----------------------

Similar to GET, by implementing the ``process_post/2`` function in
your service module, it indicates that it is able to handle POST
requests. The POST parameters are accessible to you by using
``z_context:get_q/2``.

A full example of a services which handles a POST request
is listed below::

  -module(service_something_process).
  -author("Arjan Scherpenisse <arjan@scherpenisse.net>").

  -svc_title("Processes the given id.").
  -svc_needauth(true).

  -export([process_post/2]).

  -include_lib("zotonic.hrl").

  process_post(_ReqData, Context) ->
      Id = z_context:get_q("id", Context),
      %% Do some processing here...
      Response = [{result, Id}],
      z_convert:to_json(Response).

This module could be called ``service_something_process.erl`` and then
gets served at ``/api/something/process``. It requires authentication,
and is only accessible with POST and expects an ``id`` argument to be
posted.

Again, its output is a JSON object containing a `result` field.

.. _manual-services-auth:

Service authentication
----------------------

Like stated, authentication and authorization is done either through
the Zotonic session or through a custom notification hook,
``#service_authorize{}``.

For session authentication, you need to have a valid session id (``z_sid``)
cookie. This method of authentication is the easiest when you are
accessing the services from Javascript from the same domain as your
user is logged in to.

When no session is available, but the called services requires
authentication (according to its ``svc_needauth`` metadata attribute),
a :ref:`notification hook <manual-notification>` with the name
``service_authorize`` is called.

In a default Zotonic install, this ``service_authorize`` hook is
handled by the :ref:`OAuth module <mod_oauth>`, but can be replaced by
a different service authentication module.

The module implementing the ``service_authorize`` hook is expected to
return either `undefined` (when the request is not applicable) or a
response which must conform to the Webmachine ``is_authorized/2``
return format.

.. seealso:: :term:`Services glossary entry <Service>`, :ref:`List of all core services <services>`, :ref:`mod_oauth`, :ref:`controller-api`
