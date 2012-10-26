.. _manual-template-actions:

Wiring it up: actions and postbacks
===================================

Actions are the basis of all interactivity on a Zotonic web page.

A Zotonic :ref:`Wire <scomp-wire>` is an attached handler to HTML
elements and events. A wire indicates that something should happen
when the wire is triggered. `What` should happen, is specified as the
`action`.

The :ref:`scomp-wire` scomp attached actions to to HTML elements or
Javascript events.

Actions range from a simple jQuery :ref:`action-show` to Ajax
:ref:`action-postback`\s that can trigger many other actions. The
server can also reply to a :term:`postback` or :term:`Comet` push with
actions to be executed on the browser.

A simple example is the following::

  <a href="#" id="link">Click me!</a>
  {% wire type="click" id="link" action={fade_out target="link"} %}

This :ref:`scomp-wire`\s up a link with a :ref:`action-fade_out`
action, so that when the link is clicked, it fades away.

This is obviously a trivial example, but nevertheless demonstrates the
power. These actions can be called from the template, but can also be
called based on some server-side event that occurs.

Making the templates this way is a different approach from the usual
Javascript onLoad() binding of events, but, once you're used to it,
is really simple to use and very, very powerful.


Wiring form submits
-------------------

A form is wired up in the following way::

  {% wire type="submit" id="myform" postback="form_submitted" delegate="mysite" %}
  <form id="myform" method="post" action="postback">
      <input name="username" />
      <button>Submit form</button>
  </form>

This will bind let the form submit over Ajax; the result is that a
function will be called in the specified delegate module
``mysite.erl``, called ``event/2``::

  event(#submit{}, Context) ->
      io:format("The value of 'username' is: ~s~n", z_context:get("username", Context),
      Context.


Wiring other events
-------------------

.. todo:: more examples 
   

Writing postbacks
-----------------

Whenever you want something to happen on the server from an action,
you use a `postback`.


Implementing custom actions
---------------------------

.. seealso:: listing of all :ref:`actions`.

