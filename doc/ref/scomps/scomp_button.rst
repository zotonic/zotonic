.. highlight:: django
.. include:: meta-button.rst

Makes a button with an action attached.

This is an easy way to make a button and attach one or more actions or
a postback.

For example::

{% button text="Click me" action={alert text="Hello Word!"} %}

This show a button with the text “Click me”. When clicked it will
trigger the :ref:`action-alert` action, showing an alert message with
the text “Hello World!”.

Another example::

  {% button text="Postback" postback={my_postback arg1=1 arg2=2} %}
  
.. highlight:: erlang

When clicked it will call the ``event/2`` function in the controller
that served the page. The function will be called as::

    event(#postback{message={mypostback, [{arg1,1}, {arg2,2}]},
        trigger=TriggerId, target=TargetId}, Context)

Where TriggerId and TargetId are both the HTML id of the button.

``button`` accepts the following arguments:

+----------+-------------------------------+-------------------------------+
|Argument  |Description                    |Example                        |
+==========+===============================+===============================+
|text      |The text on the button,        |text="click me"                |
|          |defaults to “Submit”           |                               |
+----------+-------------------------------+-------------------------------+
|postback  |An event sent to the delegate  |postback="hello"               |
|          |or the resource serving the    |                               |
|          |page.                          |                               |
+----------+-------------------------------+-------------------------------+
|tag       |The type of HTML tag that will |tag="a"                        |
|          |be created. Defaults to        |                               |
|          |"button".                      |                               |
+----------+-------------------------------+-------------------------------+
|delegate  |The name of the erlang module  |delegate="myresource"          |
|          |to be called for handling the  |                               |
|          |postback.                      |                               |
+----------+-------------------------------+-------------------------------+
|action    |The action to be triggered when|action={show target="msg"}     |
|          |the button is clicked.  There  |                               |
|          |can be more than one action    |                               |
|          |argument.                      |                               |
+----------+-------------------------------+-------------------------------+
|id        |Id of the button.              |id=#submit                     |
+----------+-------------------------------+-------------------------------+
|class     |The css class of the button.   |class="submit"                 |
|          |This argument can be repeated  |                               |
|          |to add multiple classes.       |                               |
+----------+-------------------------------+-------------------------------+
|style     |The css style of the button.   |style="color: #fc0"            |
+----------+-------------------------------+-------------------------------+
|tabindex  |The value for the `tabindex`   |tabindex=1                     |
|          |property.                      |                               |
+----------+-------------------------------+-------------------------------+
|type      |The type attribute of the      |type="submit"                  |
|          |button.                        |                               |
+----------+-------------------------------+-------------------------------+
|tag       |The tag used for the button.   |tag="a"                        |
|          |Defaults to 'button'.          |                               |
+----------+-------------------------------+-------------------------------+
|title     |The title attribute of the     |title="click to submit"        |
|          |button.                        |                               |
+----------+-------------------------------+-------------------------------+
|disabled  |The disabled attribute of the  |disabled=true                  |
|          |button, set to true or false.  |                               |
|          |When the button is disabled    |                               |
|          |then the class "disabled" id   |                               |
|          |added to the class list.       |                               |
+----------+-------------------------------+-------------------------------+
