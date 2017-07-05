.. _guide-services:

API services
============

Services provide a generalized way to create API
calls. These calls automatically use the authentication mechanism
(session id or :ref:`OAuth <module-mod_oauth>`) to perform access checks.

A Zotonic service is created in a separate service (sub)module. Here you
typically define the method name, the type of access (GET or POST) and if
authentication is required.

How service names are mapped to URLs
------------------------------------

The URL to call an API service is defined by the module and the method name.

In a vanilla Zotonic install, there is one single URL namespace under which all
API services can be accessed. :ref:`controller-api` by default intercepts all
URLs according to the following patterns::

    /api/:module/:method
    /api/:module

On these URLs, a lookup is done to find the corresponding Zotonic module inside
the services subdirectory of the module::

    mod_modulename/
        src/mod_modulename.erl
        src/services/
            service_modulename_methodname.erl

If no method name is used in the ``/api/:module`` URL, the method name will be
equal to the module name - see ``/api/search`` in the table below.

Examples of URLs that correspond to service handlers:

=================  ==========   ========   ====================================
URL                Module       Method     Located in service .erl file
=================  ==========   ========   ====================================
/api/base/export   mod_base     export     mod_base/src/services/service_base_export.erl
/api/base/info     mod_base     info       mod_base/src/services/service_base_info.erl
/api/search        mod_search   search     mod_search/src/services/service_search_search.erl
=================  ==========   ========   ====================================

For creating services at alternative URLs, see
:ref:`controller-api-nonstandard-url` in the :ref:`controller-api`
documentation.

Service naming in detail
........................

As stated above, a service module is defined as::

  service_<modulename>_<processname>.erl

And is then reachable on the URL
``http://<hostname>/api/<module_name>/<process_name>``.

The module *module_name* to be activated for the API call to work.

If you have a module named `mod_something` that needs a service to return stats,
your directory would look like
this::

    mod_something/
        mod_something.erl
            src/services/
                service_something_stats.erl

The url for this service will be ``http://<site_addr>/api/something/stats``

The key is that an activated module (minus the ``mod_`` prefix if you use
them!) should be part of the service name. Zotonic parses the service
modules filename to identify what module a service relates to and what
process should be called.  It checks to make sure that module is
activated and it also uses that same information when matching a
service url. So, reversly, ``service_something_stats.erl`` is served by
``http://<hostname>/api/something/stats``.

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
user’s consideration.

Secondary there is ``svc_needauth``::

    -svc_needauth(false).

This is a boolean value which tells the system whether or not a user
needs to be authorized in order to use the service.

If authentication is needed for a service, a service can only be
accessed either by using the session cookie or by using an authorized
OAuth (1.0a) token.

Creating a GET service
----------------------

By implementing the ``process_get/1`` function in your service module,
it indicates that it is able to handle GET requests.  A full example
of a services which handles a GET request is listed below::

    -module(service_something_stats).
    -author("Arjan Scherpenisse <arjan@scherpenisse.net>").

    -svc_title("Retrieve uptime statistics of the system.").
    -svc_needauth(true).

    -export([process_get/1]).

    -include_lib("zotonic_core/include/zotonic.hrl").

    process_get(_Context) ->
        Stats = [{count, 12310}, {uptime, 399}],
        z_convert:to_json(Stats).

This module could be called ``service_something_stats.erl`` and then
gets served at ``/api/something/stats``. Its output is a JSON object
containing a ``count`` and an ``uptime`` field, containing some values.

Of course, you would write real code there which retrieves actual stats. If your
module ``something`` contains the function ``stats_data/1``, call it from the
process function like this::

    process_get(Context) ->
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

    -export([process_post/1]).

    -include_lib("zotonic_core/include/zotonic.hrl").

    process_post(Context) ->
        Id = z_context:get_q(<<"id">>, Context),
        %% Do some processing here...
        Response = [{result, Id}],
        z_convert:to_json(Response).

This module could be called ``service_something_process.erl`` and then
gets served at ``/api/something/process``. It requires authentication,
and is only accessible with POST and expects an ``id`` argument to be
posted.

Again, its output is a JSON object containing a ``result`` field.

Setting response headers
------------------------

You can set response headers by returning a ``{Result, #context{}}``
tuple from the ``process_get/1`` and ``process_post/1`` calls::

    process_get(Context) ->
        Stats = mod_something:stats_data(Context),
        Result = {struct, [{count, 100}]},
        Context1 = z_context:set_resp_header(<<"cache-control">>, <<"max-age=3600">>, Context),
        {Result, Context1}.

.. _guide-services-cors:

Uploading files
---------------

The simplest way to upload files is to use the ready-made API service :ref:`service-media_upload`. But if you want to have different behavior (for instance to connect an uploaded user picture to a user page), it is easy to create your own.

The post payload should be ``multipart/form-data`` encoded (which is the standard for file uploads).

The posted data is automatically retrieved by Zotonic and made available via ``z_context``. If you use ``"upload"`` for the form data ``name`` field, you get the upload data from ``z_context:get_q(<<"upload">>, Context)``. The resulting value is an ``#upload{}`` record, and can be passed directly to ``m_media:insert_file``::

    Upload = z_context:get_q(<<"upload">>, Context),
    m_media:insert_file(Upload, Context)

Error handling
--------------

An HTTP status error code will be generated when ``process_get`` or ``process_post`` returns an error object::

        {error, error_name, DetailsString}
        {error, error_name, DetailsString, ErrorData}

Additionally, you may also ``throw()`` these error structures inside
``process_get`` and ``process_post``, to easily short-circuit your
error handling (e.g. for input validation)::

    Title = z_context:get_q(<<"title">>, Context),
    z_utils:is_empty(Title) andalso
        throw({error, missing_arg, "title"}),


Simple error feedback
.....................

By providing the error name, a corresponding HTTP status code and message will be set. Supported error names are:

=================  ===================================   ===========
Name               Generated message                     Status code
=================  ===================================   ===========
``missing_arg``    Missing argument: + Details           400
``unknown_arg``    Unknown argument: + Details           400
``syntax``         Syntax error: + Details               400
``unauthorized``   Unauthorized.                         401
``access_denied``  Access denied.                        403
``not_exists``     Resource does not exist: + Details    404
``unprocessable``  Unprocessable entity: + Details       422
(other)            Generic error.                        500
=================  ===================================   ===========

For example::

    process_post(Context) ->
        %% Do some processing here...
        case Error of
            true ->
                {{error, missing_arg, "username"}, Context};
            false ->
                {z_convert:to_json(Data), Context}
        end.


Working with Error Objects
..........................

In some cases it is useful to return more detailed error feedback. The `JSON API <http://jsonapi.org>`_ has specified a format for this. The thinking behind the format is that the server, after encountering an error, may continue to process information, and instead of returning a single error code, returns multiple found errors.

Taking this approach, this error information is returned as a JSON array, with a top key entry ``errors``::

    ["errors": {
        "detail": "...",
        "source": "...",
        "status": "...",
        "title": "..."
    }]

Of course there is no obligation to use JSON API structure, but if you want, the code of one of those functions - for instance to log on - could look like this::

    case User of
        undefined ->
            {error, [
                {status, 422},
                {source, <<"mod_webapp:logon">>},
                {title, <<"No user found">>},
                {detail, <<"Could not log on user">>}
            ]};
        _ ->
            {ok, User}
    end.

The return data of multiple functions may then be aggregated into a single error data object and returned as a list of Error Objects::

    process_post(Context) ->
        %% Do some processing here...
        %% Accumulate all data...
        %% Handle return:
        case Data of
            {error, ErrData} ->
                {{error, unprocessable, <<"">>, ErrData}, Context};
            _ ->
                {z_convert:to_json(Data), Context}
        end.

Enabling Cross-Origin Resource Sharing (CORS)
---------------------------------------------

By default the server has a
`same-origin policy <https://en.wikipedia.org/wiki/Same-origin_policy>`_:
scripts that access the API must reside on the same server.

Cross-origin resource sharing allows cross-domain requests for apps outside of
the server domain. CORS header settings define which requests are (and are not)
allowed.

In-depth background information is available at
https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

CORS settings are defined in the site's config.

**Site config settings**

``{service_api_cors, false}``
    Set to ``true`` to enable CORS

``{'Access-Control-Allow-Origin', "*"}``

``{'Access-Control-Allow-Credentials', undefined}``

``{'Access-Control-Max-Age', undefined}``

``{'Access-Control-Allow-Methods', undefined}``

``{'Access-Control-Allow-Headers', undefined}``

.. note::

    * The config file can be modified without a site restart.
    * The "Access-Control" settings only work if ``service_api_cors`` is set to true.
    * The setting name is an Erlang atom and must be in single quotes.
    * Setting values are either ``undefined`` or a string value. Multiple values can be set as a comma-separated string, for instance::

        {'Access-Control-Allow-Headers', "authorization, X-Requested-With, Content-Type"}

.. _guide-services-auth:

Service authentication
----------------------

Like stated, authentication and authorization is done either through
the Zotonic session or through a custom notification hook,
``#service_authorize{}``.

For session authentication, you need to have a valid session id (``z_sid``)
cookie. This method of authentication is the easiest when you are
accessing the services from JavaScript from the same domain as your
user is logged in to.

When no session is available, but the called services requires
authentication (according to its ``svc_needauth`` metadata attribute),
a :ref:`notification hook <guide-notification>` with the name
``service_authorize`` is called.

In a default Zotonic install, this ``service_authorize`` hook is
handled by the :ref:`OAuth module <mod_oauth>`, but can be replaced by
a different service authentication module.

The module implementing the ``service_authorize`` hook is expected to
return either `undefined` (when the request is not applicable) or a
response which must conform to the Cowmachine ``is_authorized/1``
return format.

.. seealso::

    * :term:`Services glossary entry <Service>`
    * :ref:`List of all core services <services>`
    * :ref:`mod_oauth`
    * :ref:`controller-api`
