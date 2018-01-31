.. _guide-dispatch:

Dispatch rules
==============

Dispatch rules route incoming requests to :ref:`controllers <guide-controllers>`.

A dispatch rule contains a pattern that is matched against the URL of an
incoming request. When a URL is requested by the web browser, the dispatcher
looks at the URL and matches it against all dispatch rules. Based on the match,
it calls a to handle the request.

Dispatch rules are also used for the reverse action of generating request URLs
in your templates and Erlang code. As long as you use dispatch rules in your
application, you don’t have to hard-code request URLs.

Defining dispatch rules
-----------------------

Dispatch rules are defined in a ``dispatch`` file in the ``priv/dispatch``
directory of a module or site. This ``dispatch`` file contains a list of
dispatch rules. For example:

.. code-block:: erlang
    :caption: sites/yoursite/priv/dispatch

    %% Example dispatch rules
    [
        {home,      [],                         controller_page,  [ {template, "home.tpl"}, {id, page_home} ]},
        {features,  ["features"],               controller_page,  [ {template, "features.tpl"}, {id, page_features} ]},
        {collection,["collection", id, slug],   controller_page,  [ {template, "collection.tpl"} ]},
        {category,  ["category", id, slug],     controller_page,  [ {template, "category.tpl"} ]},
        {documentation, ["documentation", id, slug], controller_page, [ {template, "documentation.tpl"} ]}
    ].

Each dispatch rule is a tuple that contains four elements:

1. The dispatch rule name (e.g. ``collection``).
2. The match pattern for the request URL’s path (e.g., ``collection/{id}/{slug}``).
3. The controller that will handle the request (:ref:`controller-page` above).
4. An optional list of controller arguments (in our
   example above, the template name to be rendered).

Let’s look at each of these elements in turn.

1. Dispatch rule name
^^^^^^^^^^^^^^^^^^^^^

Each dispatch rule must have a name. This name is used for
:ref:`generating URLs <guide-dispatch-generating>`.

Dispatch rules do not have to be unique: if multiple rules with the same name
exist, the dispatcher will look at the first rule that matches both the name and
the optional extra arguments that were given.

2. Match pattern
^^^^^^^^^^^^^^^^

In your match patterns, use:

- strings to define fixed parts that must match the URL (e.g. ``collection/``)
- atoms to define dispatch rule arguments (e.g. ``id`` and ``slug``) that will
  bind to the URL part at that position
- the special atom ``'*'`` to bind to the remaining part of the URL. This must
  be the last element of the path.

So a request to ``http://yoursite/collection/123/nice-slug-you-got-there``
matches the ``collection`` dispatch rule above, with the following bindings:

======== =======================
argument value
======== =======================
id       123
slug     nice-slug-you-got-there
======== =======================

You can access the values with the ``q`` template variable::

    {{ q.id }}
    {{ q.slug }}

To view all variables, use the :ref:`debug tag <scomp-debug>`.

Order of rules
""""""""""""""

Dispatch rules are processed from top to bottom, so make sure the more specific
rules are above more general ones.

Match pattern constraints with regular expressions
""""""""""""""""""""""""""""""""""""""""""""""""""

Some developers need very particular control of dispatch in order for
their applications to function as they want them to.

Say you want to only accept numerical arguments as an id in::

  {foo, ["foo", id], controller_foo, []}

The you can use a dispatch rule with a regular expression test::

  {foo, ["foo", {id, "^[0-9]+$"}], controller_foo, []}

or, you can specify some extra options::

  {foo, ["foo", {id, "1?2?", [notempty]}], controller_foo, []}

In this case, the id must contain a 1 or a 2, amongst any other characters.

3. Controller
^^^^^^^^^^^^^

The third element of the dispatch rule is the controller that will handle the
incoming request. This can be one of Zotonic’s :ref:`built-in controllers <controllers>`
or a :ref:`custom controller <guide-controllers>`.

4. Controller arguments
^^^^^^^^^^^^^^^^^^^^^^^

The last element is an optional property list that will be passed as arguments
to the controller. Refer to the :ref:`documentation for each controller <controllers>`
for available arguments.

.. _guide-dispatch-generating:

Generating URLs
---------------

In templates
^^^^^^^^^^^^

To generate URLs in templates, use the :ref:`url tag <tag-url>` and pass the
dispatch rule name::

    {% url home %}

And with dispatch rule arguments::

    {% url collection id=123 slug="nice-slug-you-got-there" %}

which gives: ``/collection/123/nice-slug-you-got-there``.

Any arguments that are not defined in the dispatch rule are appended as query
string parameters, so::

    {% url features var=1 x="hello" %}

will result in the URL ``/features?var=1&x=hello``.

In Erlang
^^^^^^^^^

To generate URLs in your Erlang code, use ``z_dispatcher``:

.. code-block:: erlang

    z_dispatcher:url_for(features, Context).

    %% and with dispatch rule arguments:
    z_dispatcher:url_for(collection, [{id, 123}, {slug, "nice-slug-you-got-there"}], Context).

Dispatcher details
------------------

Organizing dispatch files
^^^^^^^^^^^^^^^^^^^^^^^^^

A module or site can have multiple dispatch files, and they can have
any filename as long as you don’t use the extension ``.erl``.

The module indexer will load all dispatch files. They can be reloaded
with the “rescan” button in the admin modules page. Illegal dispatch
files are skipped, showing an error message in the Zotonic shell.

When your dispatch rules don't work, check first if there are any
typos, then check if your dispatch rules are not overruled by a module
that loads earlier. Modules are loaded on priority first, then on
module name.

.. _guide-dispatch-rewriting:

URL rewriting
^^^^^^^^^^^^^

Before URLs are matched, they can be rewritten to match something else. This is
done using the :ref:`dispatch_rewrite notification <dispatch_rewrite>`. This
allows one to set extra context variables or change the (internal) URL so
different dispatch rules get triggered.

An application of URL rewriting allows you to set the Zotonic language based on
the domain that is being requested on your site. To set up domain-based language
detection using the following code snippet::

    observe_dispatch_rewrite(#dispatch_rewrite{host=Host}, {Parts, Args}, _Context) ->
        Language = case Host of
            "example.nl" -> nl;
            "example.de" -> de;
             _ -> en  %% default language
         end,
        {Parts, [{z_language, Language} | Args]}.

This leaves the request URI intact (the ``Parts`` variable), but injects
the ``z_language`` variable into the request context, overriding the language
selection.

For this setup to work, this requires you to have the ``{redirect,
false}`` option in your site, and the appropriate ``hostalias``
directives for each host. See :ref:`guide-site-anatomy` for more
details.

Unmatched hosts/domains
^^^^^^^^^^^^^^^^^^^^^^^

First, the dispatcher finds the site that matches the HTTP host in the
request. If no site can be found then the dispatcher will first check all
enabled sites with a :ref:`dispatch_host notification <dispatch_host>` to see if
any site has a known redirect.

If this fails then the dispatcher will select a default site
(usually the :ref:`status site <ref-status-site>`) to handle the request.

If no site is running at all then a bare bones ‘404 Not Found’ page will be
shown.

See :ref:`mod_custom_redirect` for redirecting unknown domains.

Unmatched paths
^^^^^^^^^^^^^^^

If the dispatcher has found a site to handle the request, it looks for a
dispatch rule that matches the request path. If no such rule can be found,
a :ref:`dispatch notification <dispatch>` is triggered.

The module :ref:`mod_base` will check the request path against the ``page_path``
property of all resources. After that the module :ref:`mod_custom_redirect` will
check the configured redirect locations.

.. seealso:: :ref:`mod_custom_redirect`, :ref:`mod_base`
