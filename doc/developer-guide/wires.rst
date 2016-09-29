.. _guide-wires:

Wires
=====

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
