
.. include:: meta-wire.rst

Connect actions and events to a HTML element.

The wire tag is the basis for most Ajax interaction on web pages. It allows to connect actions to HTML elements. Examples of :ref:`actions` are :ref:`action-show` / :ref:`action-hide` elements or :ref:`action-postback` to the server.

Wire actions to an element
--------------------------

The primary use of wire is to connect an action to a HTML element or javascript event.

Example::

   {% wire id="show" action={show target="message"} %}
   <a id="show" href="#">Click to show a message</a>
   <p style="display: none" id="message">Hello World!</p>

A click on the link will show the message “Hello World!”.


Wire postbacks to the server
----------------------------

Wire can post an event back to the server. Use for this the “postback” argument::

   {% wire id="mybutton" postback={hello world="round"} %}
   <button id="mybutton">Post event to server</button>

When the button is clicked the event function of the page controller will be called as::

   event({postback, Postback, TriggerId, TargetId}, Context).

Where “Postback” will be ``{hello, [{world,"round"}]}`` and both “TriggerId” and “TargetId” will be "mybutton".


Wire form submit events
-----------------------

Wire is also used to connect a form to the event routine handling the form.

Example::

   {% wire id="myform" type="submit" postback="some_tag" action={toggle target="message"} %}
   <form id="myform" method="post" action="postback">
     <input type="text" name="title" value="" />
     <button id="mybutton" type="submit">Submit</button>
   </form>

The wire tag redirects the submit of the form to the event routine of the controller. A submit will also toggle the visibility of the "message" element on the page. Note that the action is “postback”, this is obligatory.

The event routine will be called as::

   event({submit, Tag, FormId, TargetId}, Context).

Where Tag will be the postback set by the wire tag (in the example the atom 'some_tag') and FormId and TargetId are both the HTML id of the submitted form.  The posted input fields can be fetched using ``z_context:get_q/2``, ``z_context:get_q_all/1`` or ``z_context:get_q_validated/2``.

::

   Title = z_context:get_q("title", Context),
   AllArgs = z_context:get_q_all(Context);

There are some extra arguments added to every form post:

+---------------+------------------------------------------------------------+-----------------------+
|Post argument  |Description                                                 |Example                |
+===============+============================================================+=======================+
|z_trigger_id   |Id of the HTML element that triggered the submit.           |"mybutton"             |
+---------------+------------------------------------------------------------+-----------------------+
|z_pageid       |Id of the page in the browser, used to connect comet and    |"1uTsbzIsWqmdPmNopF32" |
|               |other communictation between the browser and the server.    |                       |
+---------------+------------------------------------------------------------+-----------------------+
|postback       |Signed postback set by the wire tag. Handled internally.    |                       |
+---------------+------------------------------------------------------------+-----------------------+


Wire to the page load or unload
-------------------------------

A ``{% wire %}`` without an id will bind the actions and/or postback to the window instead of an element. Omitting a type as well will execute all actions and/or postback on page load.

Example::

   {% wire action={alert text="Welcome to this page."} %}

The type “unload” will execute all actions and/or postback when leaving the page::

   {% wire type="unload" action={alert text="Bye."} %}


Arguments
---------

The wire tag accepts the following arguments:

+---------------+-----------------------------------------------------------------------+-------------------------------------+
|Argument       |Description                                                            |Example                              |
+===============+=======================================================================+=====================================+
|id             |HTML id of the element the action gets connected to. When the id is not|id="mybutton"                        |
|               |given then the event is bound to the window.                           |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|type           |The type of the event triggering the action. Defaults to "click". Other|type="submit"                        |
|               |types are: "enterkey", "interval", "continuation", "submit" or one of  |                                     |
|               |the jQuery events "blur", "focus", "load", "resize", "scroll",         |                                     |
|               |"unload", "beforeunload", "click", "dblclick", "mousedown", "mouseup", |                                     |
|               |"mousemove", "mouseover", "mouseout", "mouseenter", "mouseleave",      |                                     |
|               |"change", "select", "keydown", "keypress", "keyup" or "error".         |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|propagate      |Specify this when you don’t want the event to be canceled after        |propagate                            |
|               |handling the wire.  Useful for event types like focus, click etc.      |                                     |
|               |.. versionadded:: 0.6.1                                                |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|target         |Possible target for the action. The meaning of this argument depends on|                                     |
|               |the action, defaults to id.                                            |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|action         |Action wired to the element. This parameter can be repeated to wire    |action={toggle                       |
|               |more than one action at a time. The value is a single or a list of     |target="message"}                    |
|               |action records.                                                        |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|postback       |Postback that will be sent to the event handler of the controller or   |postback="ajaxevent"                 |
|               |the delegate.  Either a string, which will be send as an atom, or a    |postback={myevent foo=1 bar=2}       |
|               |tagged property list.  The example will be in Erlang ``{myevent,       |                                     |
|               |[{foo,1},{bar,2}]}``.                                                  |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+
|delegate       |Name of the Erlang module that will receive the postback. Defaults to  |delegate="event_handler"             |
|               |the controller that handled the page request.                          |                                     |
+---------------+-----------------------------------------------------------------------+-------------------------------------+

.. seealso:: the tag :ref:`scomp-wire_args` and the list of predefined :ref:`actions`.

