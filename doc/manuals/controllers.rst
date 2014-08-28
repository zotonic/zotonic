.. _manual-controllers:

URL Controllers
===============

:term:`Controllers <Controller>` are the Erlang modules which decide
what happens when a browser requests a page. Zotonic looks at the
:term:`dispatch rules <Dispatch rule>` that match the requested URL,
and if a dispatch rule matches, the controller that is named in the
dispatch rule is used to handle the request.

Internally, Zotonic uses a fork of `Webmachine
<http://wiki.basho.com/Webmachine.html>`_ for handling the
requests. What Zotonic calls a `controller`, Webmachine calls a
`resource`. See also :ref:`manual-controllers-webzmachine`.


Anatomy of a controller
-----------------------

Say we have the following dispatch rule, handling ``http://localhost/example``::

  {example_url, ["example"], controller_example, []},

When hitting ``/example``, the `controller_example` controller will be
initialized and various callback functions on the controller will be
called, according to the HTTP protocol flow.

Controllers are pretty self-documenting, thanks to the names of the
webmachine callback functions. For instance, when you define a
function ``resource_exists/2``, it will be called to decide whether or
not the page should return a 404 page.

The simplest controller uses Zotonic’s ``controller_html_helper.hrl`` include to serve HTML:

.. code-block:: erlang  

   -module(controller_example).

   -include_lib("controller_html_helper.hrl").

   html(Context) ->
       {<<"<h1>Hello</h1>">>, Context}.
       
To return the rendered output of a template file in the module's 
``templates`` directory, use ``z_template:render/3``:

.. code-block:: erlang  

   -module(controller_example).

   -include_lib("controller_html_helper.hrl").

   html(Context) ->
       % foo and bam will be available as template variables in mytemplate.tpl.
       Html = z_template:render("mytemplate.tpl", [{foo, 'bar'}, {bam, 1234}], Context),
       z_context:output(Html, Context).       

If you need more examples, :ref:`mod_base` contains many controllers,
implementing basic HTTP interaction but also redirects, websockets, et
cetera. The :ref:`controllers` page lists all available controllers in
the Zotonic core. 
       
The `Webmachine documentation
<http://wiki.basho.com/Webmachine-Demo.html>`_ itself is also very
helpful for understanding controllers.



.. _manual-controllers-webzmachine:

Differences between Zotonic’s and Basho’s Webmachine
----------------------------------------------------

Zotonic’s fork has been named ``webzmachine`` and lives in its
separate repository at https://github.com/zotonic/webzmachine).

The main differences with Basho’s Webmachine are:

* Pluggable dispatch handler
* Support for the HTTP ``Upgrade:`` header
* Optional caching of controller callbacks results
* Dispatch handler can redirect requests
* Use of process dictionary has been removed
* ``webmachine_request`` is now a normal (not parametrized) module
* Extra logging

Alltogether, this gave a significant speed boost to Webmachine.

In the specific case of Zotonic the difference was 5 milliseconds (or
more) per request (on a 2GHz Core 2 Duo). Without these optimizations
we were not able to use Webmachine.
