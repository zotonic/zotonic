
.. include:: meta-live.rst

Live updating templates connected to :ref:`MQTT topics <mod_mqtt>`.

.. note::

    The live tag is provided by :ref:`mod_mqtt`, which must be enabled.

This tag renders templates that are automatically re-rendered after a
publication to an MQTT topic.

Example
-------

.. highlight: django

An example of a template showing the newest content of a resource::

    {% live template="_detail.tpl" topic=id id=id %}

This renders the template ``_detail.tpl``. If the resource with id ``id`` is
updated then the template will be replaced with a freshly rendered template.

The tag can subscribe to multiple topics at once.

Add the argument ``catinclude`` to do a :ref:`tag-catinclude` instead of a
normal :ref:`tag-include`. For a catinclude the argument ``id`` must be present::

    {% live template="_detail.tpl" topic=id catinclude id=id %}

Live topics
-----------

Any MQTT topic can be used. The topics are interpreted as local to the page.
There are three special topics:

* Use any integer to map to the resource’s update topic. For example if id is ``1234``
  then the topic will be ``bridge/origin/model/rsc/event/1234``
* Use the tuple ``{object id=...}`` to listen to changes of outgoing connections from a page.
  An example of a mapped topic is `bridge/origin/model/edge/event/1234/o/+``.
  Use the tuple ``{object id=... predicate=...}`` to listen to changes of a specific predicate of a page.
  An example of a mapped topic is ``bridge/origin/model/edge/event/1234/o/author``
* Use the tuple ``{subject id=... }`` to listen to changes of incoming connections to a page.
  An example of a mapped topic is ``bridge/origin/model/edge/event/1234/s/author``

Note that the topics refer to *client side topics*, that is why the bridge is used
to subscribe to server side model events.

It is possible to subscribe to client topics like ``"my/local/topic"`` and have the actions
triggered by publish to ``cotonic.broker.publish("my/local/topic", {});`` (with any payload).


Live actions
------------

It is possible to wire actions or postbacks to a MQTT topic.

Use the :ref:`wire tag <scomp-wire>` with argument
``type={mqtt topic=... topic=...}`` to connect to one or more MQTT topics:

.. code-block:: django

    {% wire type={mqtt topic="bridge/origin/public/hello"} action={growl text="hello"} %}

And in Erlang this will trigger the above *growl*::

    z_mqtt:publish(<<"public/hello">>, <<>>, z:c(mysite)).
