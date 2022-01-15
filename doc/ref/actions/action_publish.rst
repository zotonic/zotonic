
.. include:: meta-publish.rst

Publishes a message on the topic tree of the current page.

Example::

   {% button text="Hello" action={publish topic="some/topic" js_msg="Hello World!"} %}

When clicked, the message set to the message ``"Hello World!"`` is published.

Another example, now publishing a more complex message::

  {% button text="hello" action={publish topic="some/topic" foo="bar" spam="eggs"} %}

When clicked, the message set to the message ``{foo: "bar", spam: "eggs"}`` is published.

Prefix the topic with ``bridge/origin/`` to relay it to the topic tree on the server.


===========  ===============================================================  ==========================
Argument     Description                                                      Example
===========  ===============================================================  ==========================
topic        The topic of the message.                                        `topic="test"`
js_msg       Literal javascript message to publish. Will be escaped.          `js_msg="Hello World"`
<key>        A literal javascript value, will be escaped and added to object  `spam="eggs"`
===========  ===============================================================  ==========================

