.. highlight:: django
.. _guide-templates:

Templates
=========

Zotonic’s template syntax is very similar to the Django Template Language (DTL).

The templates in Zotonic are based on the Django Template Language
(DTL), using a customized version of the excellent `ErlyDTL
<https://github.com/evanmiller/erlydtl>`_ library. Over the years,
Zotonic’s version of ErlyDTL has diverged, adding Zotonic-specific
features and more powerful expression possibilities. However, the main
template syntax remains the same:

The double accolade/brace construct outputs the value of the
variable::

    {{ foo }}

Optionally, you can pipe these variables through a so-called filter,
which is applied before output::

    {{ foo|lower }}

Template tags allow you to express complex constructs like loops and
branches::

    {% if username == 'Arjan' %} Hi, Arjan {% endif %}


.. _guide-lookup-system:

Template locations and the lookup system
----------------------------------------

All templates are located in the :file:`templates` directory of
modules.  Templates are referred to by their full filename. When a
template is inside a directory then the full path of the template must
be given.

For example, say we have two templates:

  | mod_example/templates/foobar.tpl
  | mod_example/templates/email/email_base.tpl

The above are referred to as ``foobar.tpl`` and
``email/email_base.tpl``.  Just ``email_base.tpl`` will not find the
email template.

All templates of all modules are grouped together, regardless of which
module they are defined in. The module name is never given as part of
the template name.

Module priority and overriding templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Templates with the same filename can be defined in multiple
modules. The actual template which is selected depends on the priority
of the module.

The :dfn:`module priority` is a number defined in the module’s code
and is usually a number between 1 and 1000.  A lower number gives a
higher priority.  Templates in a module with higher priority hide
templates in a module with lower priority.

When two modules have the same priority then the modules are sorted by
their name.  That means that, given the same priority number,
``mod_aloha`` has higher priority than ``mod_hello``.

This mechanism allows any module (or site) to replace every single
template in the system with its own version.

.. note:: Including all similar named templates

    | The tag ``{% all include "foobar.tpl" %}`` will include all
      templates named :file:`foobar.tpl`.
    | Where the tag ``{% include "foobar.tpl" %}`` only includes the
      highest priority :file:`foobar.tpl`.

    See also :ref:`tag-all-include` and :ref:`tag-all-catinclude`.

Module priority and overriding lib files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Exactly the same module priority is also valid for all files in the
:file:`lib` directory of modules.

This allows any module to change the static css, javascript, images,
favicon.ico, robots.txt and other static files with its own version.


.. _guide-lookup-system-ua:

User Agent selection
^^^^^^^^^^^^^^^^^^^^

The module priority is a very powerful mechanism for extending and
adapting Zotonic.

But what if a page requested with a mobile phone should be served with
a different template than the same page requested with a desktop
computer?

For this there is another template selection mechanism, based on the
categorization of the device requesting the page.

User agent classes
""""""""""""""""""

Every request Zotonic classifies the device using the *User-Agent*
request header. The possible classifications are:

 text
   Screen readers, feature phones, text only browsers.

 phone
   Smart phones, capable of javascript and having a touch interface or
   other pointing device.

 tablet
   Big screen, javascript, modern browser and touch interface.

 desktop
   Big screen, javascript, modern browser and pointing device.

The selected class is available in ``m.req.ua_class`` or from Erlang
``z_user_agent:get_class/1``.

.. note:: More properties can be found using ``m.req.ua_props`` or
          ``z_user_agent:get_props/1``.

The four user agent classes map to subdirectories of the
:file:`templates` directory:

  | mod_example/templates/desktop/...
  | mod_example/templates/phone/...
  | mod_example/templates/tablet/...
  | mod_example/templates/text/...

All templates that are not in those sub-directories are categorized as
*generic*.

Lookup by user agent class
""""""""""""""""""""""""""

The template system follows a strict hierarchy between the different
user agent classes:

	desktop → tablet → phone → text → generic

Where the system starts looking from the current user agent class to
the right.  So for a phone, the templates in the :file:`tablet` and
:file:`desktop` directories will never be considered.

Combination of user agent and module priority
"""""""""""""""""""""""""""""""""""""""""""""

The user agent class and the module priority are two dimensions of the
template selection process.

The module priority is more important than the user agent class.

A mismatch in user agent class (e.g. a desktop template when looking
for a phone version) will never be selected.  A sub-optimal version
(e.g. a generic or text version instead of a phone version) will be
selected if that sub-optimal version resides in a module with higher
priority than the module with the better matching version.

The *all include* tag will select the best version from all
modules. Again skipping any user agent mismatches.


.. note:: Building templates and mobile first.

    The lookup strategy for templates conforms to a *mobile first*
    strategy.  When adding a page or building a site, the idea is to
    start with the simplest, text only, version of the site.  The text
    only version is then placed in the :file:`templates/text`
    directory.  Next will be adding more features, markup and
    interaction for the phone version.  Only then moving up to the big
    screen for tablet (touch) or desktop (mouse).


.. note:: Seeing which template is selected.

    `mod_development` implements a screen where it is possible to see
    in real time which templates are included and compiled. The full
    path of all templates can be seen, giving insight in the template
    selection process.

    See also :ref:`mod_development`

.. _guide-template-variables:

Template variables
------------------

.. _template-magicvalues:

Global variables
^^^^^^^^^^^^^^^^

The following properties are always available in a template.

zotonic_dispatch
    The name of the dispatch rule that was applied to render the current page.

zotonic_dispatch_path
   A list containing the request path used as initial input for the dispatcher.
   The path is split on ``/`` and after an optional rewrite. This means that the
   list doesn’t contain the language prefix. For example, the path
   ``/en/foo/bar?a=b`` will give the list ``["foo", "bar"]``.

zotonic_dispatch_path_rewrite
  Same as zotonic_dispatch_path, but set to the path after an optional internal
  request rewrite inside the dispatcher. For example if a resource has its
  `page_path` set to ``/foo`` and the requested path is ``/en/foo`` then the
  ``zotonic_dispatch_path`` will be set to ``["foo"]`` and the
  ``zotonic_dispatch_path_rewrite`` could be set to something like
  ``["page", "1234", "foo-slug"]``.

z_language
    The currently selected language. This an atom, for example: ``en``.

q
    A dictionary containing the current request's query variables. For GET requests, these are the arguments passed from the query string (e.g. ``?foo=bar``); for POST requests, these are the values posted in the POST form. For more access to the raw request data, look at the :ref:`model-req` model.

now
    The local date and time in Erlang tuple notation, for instance ``{{2014,4,17},{13,50,2}}``.

m
    ``m`` is not really a value, but it's an indicator to trigger a lookup in one of Zotonic's :ref:`models`. For instance the :ref:`model-rsc` model is always exposed and can be used like this ``{{ m.rsc[123].title }}``.

z_trigger_id
   Only available in postback contexts. The id of the html element triggering a postback.

z_target_id
   Only available in postback contexts. The id of the html element that is the target of a postback.

z_delegate
   Only available in postback contexts. The name of the Erlang module handling the postback event.


Besides these variables, all key/value pairs that are set in the
``#context{}`` record (using ``z_context:set/2``) that was used to
render the current template are also exposed into the template's
global scope.

.. _guide-tags:

Tags
----

Tags add logic and flexibility to your templates. The general syntax for a tag
is the following::

    {% tagname param1=value param2=value %}

Some tags are *block tags* and therefore consist of a start and an end
tag. The name of the end tag is always ``end`` plus the name of the
opening tag::

    {% tag %}
        ...
    {% endtag %}

For instance, use the ``for`` tag to loop over lists::

    {% for article in articles %}
        {{ article.title }}
    {% endfor %}

And the ``if`` tag to check conditions::

    {% if article.is_published %}
        There you go: {{ article.title }}
    {% else %}
        Sorry, the article hasn’t been published yet!
    {% endif %}

.. seealso::

    * List of :ref:`all tags <tags>` reference.
    * :ref:`Create your own tags cookbook <cookbook-custom-tag>`.

.. _guide-filters:

Filters
-------

Filters are used to modify values you want to show or use in your templates. For
example::

    {{ value|lower }}

will lowercase the input value using the :ref:`filter-lower` filter.

.. seealso:: a listing of all :ref:`filters <filters>`.

.. _guide-models:

Models
------

A template model provides data to a template through the syntax:
``m.modelname.property``. For example::

    {# Get the site's title #}
    {{ m.site.title }}

    {# Fetch the title of the page with name page_home #}
    {{ m.rsc.page_home.title }}

    {# Fetch the title of the page whose id is the integer 1 #}
    {{ m.rsc[1].title }}

    {# Fetch the title of the page whose id is the template variable id #}
    {{ m.rsc[id].title }}

    {# Perform a search on all persons #}
    {% for p in m.search[{query cat='person'}] %}{{ p.title }}{% endfor %}

.. seealso::

    * list of :ref:`all models <models>` in the reference
    * :ref:`cookbook-custom-model` cookbook

.. _guide-media:

Media
-----

To include a resource’s depiction, use :ref:`tag-image`::

    {% image id %}

You can pass extra parameters to adjust the image on the fly::

    {% image id width=200 height=200 crop %}

The image will then be resized and cropped to the specified 200x200 pixels.

.. seealso:: :ref:`tag-image` for all parameters

.. _guide-media-classes:

Media classes
^^^^^^^^^^^^^

Instead of inline image tag parameters, you can use media classes to define
image transformations. The advantage is that this image definition can then be
reused amongst templates.

Create a ``templates/mediaclass.config`` file in your site directory:

.. code-block:: erlang

    [
        {"thumb", [
            {width, 200},
            {height, 200},
            crop
        ]}
    ].

This defines a media class called ‘thumb’, which can be used to display a
120x120 cropped square image. You then only need to refer to this media class in
your image tag::

    {% image id mediaclass="thumb" %}

The image URL will have a checksum embedded in it so that when the contents of
the media class is changed, all images which use that media class will be
regenerated to reflect the new media class.

Raw ImageMagick options
"""""""""""""""""""""""

Besides the normal image processing options, as described in :ref:`tag-image`,
it is possible to add literal ImageMagick convert commands to the mediaclass
definition.

For example::

    {magick, "-level 90%,100% +level-colors \\#FE7D18,\\#331575"}

(Note that you have to double any backslashes that were needed for the
``convert`` command line.)

This command is given *as-is* to the ImageMagick `convert` command, therefore it
is best to first try it with the command-line `convert` command to find the
correct options and command line escapes needed.

There are three variations: ``pre_magick``, ``magick``, and ``post_magick``.
The only difference is that the ``pre_magick`` is added before any other filter
argument, ``magick`` somewhere between, and `post_magick` after the last filter.

In this way it is possible to pre- or post-process an image before or after
resizing.

See http://www.imagemagick.org/Usage/ for examples of using ImageMagick from the
command line.

User-agent specific images
""""""""""""""""""""""""""

Since ``mediaclass.config`` files are found using the
:ref:`guide-lookup-system`, it is subject to the same selection rules that
normal templates fall under.

The consequence is that you can have multiple ``mediaclass.config``
files, for instance one in `desktop/`, one in `phone/`. The media
classes defined in those subdirectories can have the same names. This
way you can make thumbnail sizes smaller for phones, or serve
higher-quality JPEG file for desktop browsers.

See :ref:`guide-lookup-system-ua` for the details on the user-agent
selection mechanism.

.. _guide-actions:

Actions
-------

The action defines what should happen when the wire is triggered. Actions can
be client-side (such as JavaScript animations) or server-side postbacks.

Trigger actions from JavaScript
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To trigger an action from an HTML element, you attach a wire to the element::

    <a href="#" id="link">Click me!</a>
    {% wire type="click" id="link" action={fade_out target="link"} %}

The wire’s ``id`` value must match the ``id`` value of the HTML element. This
wires up a link with a :ref:`action-fade_out` action, so that when the link
is clicked, it fades away.

Actions can be called from the template, but can also be called when some
server-side event occurs.

.. seealso:: :ref:`guide-template-autoids`, :ref:`cookbook-custom-action`

Server postbacks
^^^^^^^^^^^^^^^^

Postbacks are server-side actions. For instance, to submit a form asynchronously
through Ajax, use a postback::

    {% wire type="submit" id="myform" postback="form_submitted" delegate="mysite" %}
    <form id="myform" method="post" action="postback">
        <input name="username" />
        <button>Submit form</button>
    </form>

This will submit the form over Ajax; the result is that a function will be
called in the specified delegate module ``mysite.erl``, called ``event/2``:

.. code-block:: erlang

    event(#submit{}, Context) ->
        io:format("The value of 'username' is: ~s~n", z_context:get("username", Context),
        Context.

.. seealso:: :ref:`postback reference <action-postback>`

Trigger browser actions from the server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. todo::

.. seealso:: listing of all :ref:`actions <actions>`.

Named actions
^^^^^^^^^^^^^

If you want to trigger actions from your JavaScript code, give the action a
name::

    {% wire name="my_action" action={growl text="Hello World"} %}

You can then refer to it in your JavaScript code:

.. code-block:: javascript

    z_event("my_action");

And pass arguments to the action:

.. code-block:: javascript

    z_event("my_action", { foo: bar });

The argument ``foo`` will become a query argument, that you can access in your
Erlang module with ``z_context:get_q(foo, Context)``.

Adding CSS and JavaScript
-------------------------

JavaScript
----------



.. _guide-template-autoids:

Auto-generated identifiers
--------------------------

If you include a template many times (i.e. from a for loop), then having
fixed element identifiers are no good. Zotonic provides a mechanism to generate
an identifer which has a unique value within the template.

To prefix the id with a unique value (per invocation of the
template) prefix the id with a ``#``-sign:

.. code-block:: html

    <div id="{{ #foo }}">

This special notation will replace ``#foo`` with an auto-generated
identifer, which will expand to something like this:

.. code-block:: html

    <div id="ubifgt-foo">

Unique ids can also be generated inside a ``for`` loop:

.. code-block:: html

    {% for id in mylist %}
        <li id="{{ #foo.id }}">{{ id.title }}</li>
    {% endfor %}

This will generate HTML like this:

.. code-block:: html

  <li id="gdjqa-foo-1234">Some great news</li>

When using a :ref:`scomp-wire` tag, that same unique id can be referenced:

.. code-block:: html

    {% for id in mylist %}
        <li><a id="{{ #list.id }}" href="#">{{ m.rsc[id].title }}</a></li>
        {% wire id=#list.id action=some_action %}
    {% endfor %}

.. _guide-icons:

Icons in templates
------------------

Zotonic provides a couple of ways to show icons in templates:

* :ref:`mod_artwork` gives access to FontAwesome and Material Design icons.
  It also has a number of other icon collections, mostly PNG images. Activate
  the module and follow the instructions on the doc page.
* Zotonic icons provided by `mod_base`. This is explained on the current page.

To create a certain amount of consistency across modules, Zotonic comes with a
small set of commonly used icons and CSS classes (edit, help, close, etcetera)
plus the Zotonic logo.

Use cases:

* You create your frontend from scratch, but you also have pages in your site
  that are provided by other modules, for instance the login screens. It would
  be good if the social login icons show up.
* You are writing a template or module and like to take advantage of ready
  available icons.
* You are writing frontend styles in LESS and you would like to extend Zotonic
  / FontAwesome / Material Design icons.

Include the Zotonic icons CSS file in your template::

    {% lib
        "css/z.icons.css"
    %}

Then use this syntax in your template HTML::

    z-icon z-icon-<name>

For instance::

    <span class="z-icon z-icon-off"></span>

.. seealso:: :ref:`ref-icons` reference

