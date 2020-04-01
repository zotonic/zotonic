.. _guide-controllers:

Controllers
===========

:term:`Controllers <Controller>` are the Erlang modules which decide
what happens when a browser requests a page. Zotonic looks at the
:term:`dispatch rules <Dispatch rule>` that match the requested URL,
and if a dispatch rule matches, the controller that is named in the
dispatch rule is used to handle the request.

Anatomy of a controller
-----------------------

Say we have the following dispatch rule, handling ``https://localhost/example``::

  {example_url, [ "example" ], controller_example, []},

When hitting ``/example``, the ``controller_example`` controller will be
initialized and callback functions on the controller will be
called, according to the HTTP protocol flow.

Controllers are pretty self-documenting, thanks to the names of the
cowmachine callback functions. For instance, when you define a
function ``resource_exists/1``, it will be called to decide if
the page should return a 404.

The simplest controller to serve HTML::

   -module(controller_example).

   -export([ process/4 ])

   process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
       {<<"<h1>Hello</h1>">>, Context}.

.. _guide-render:

Rendering templates
-------------------

To return the rendered output of a template file in the module's
:file:`priv/templates` directory, use ``z_template:render_to_iolist/3``::

   -module(controller_example).

   -export([ process/4 ])

   process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
       % foo and bam will be available as template variables in mytemplate.tpl.
       Vars = [
          {foo, <<"bar">>},
          {bam, 1234}
       ],
       z_template:render_to_iolist("mytemplate.tpl", Vars, Context).

If you need more examples, :ref:`mod_base` contains many controllers,
implementing basic HTTP interaction but also redirects, websockets, et
cetera. The :ref:`controllers` page lists all available controllers in
the Zotonic core.

The `Cowmachine documentation
<https://github.com/zotonic/cowmachine/wiki>`_ is
helpful for understanding controllers.

.. _guide-controllers-cowmachine:

Differences between Cowmachine and Basho’s Webmachine
-----------------------------------------------------

Zotonic’s fork of Webmachine has been named ``cowmachine`` and lives in its
separate repository at https://github.com/zotonic/cowmachine).

The main differences with Basho’s Webmachine are:

* Uses Cowboy instead of MochiWeb
* All callbacks have a single handler
* Pluggable dispatch handler
* Support for the HTTP ``Upgrade:`` header
* Optional caching of controller callbacks results
* Dispatch handler can redirect requests
* Use of process dictionary has been removed
* ``webmachine_request`` is now a normal (not parametrized) module
* Extra logging
* ping and init callbacks are removed

Together, this is a significant simplification and speed boost.

