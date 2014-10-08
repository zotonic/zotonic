
.. include:: meta-live.rst

Live updating templates connected to MQTT topics.

This scomp renders templates that are automatically rerendered after a publication to a MQTT topic.

.. note::

    This scomp is provided by :ref:`mod_mqtt`, this must be enabled.


Example
-------

.. highlight: django

An example of a template showing the newest content of a resource::

    {% live template="_detail.tpl" topic=id id=id %}

This renders the template ``_detail.tpl``. If the resource with id *id* is updated then the template
will be replaced with a freshly rendered template.

The scomp can subscribe to multiple topics at once.


Live topics
-----------

Any MQTT topic can be used. The topics are interpreted as local to the page.
There are three special topics:

 * Use any integer to map to the resource’s update topic. For example if id is *1234* then the topic will be ``~site/rsc/1234``
 * Use the tuple ``{object id=... predicate=...}`` to listen to changes of a specific predicate of a page. An example of a mapped topic is ``~site/rsc/1234/o/author``
 * Use the tuple ``{object id=...}`` to listen to changes of a all predicates of a page. An example of a mapped topic is ``~site/rsc/1234/o/+``
 * Use the tuple ``{subject id=... }`` to listen to changes of connections to a page. An example of a mapped topic is ``~site/rsc/1234/s/author``


Live actions
------------

It is possible to wire actions or postbacks to a MQTT topic.

Use ``{% wire type={mqtt topic=... topic=...} %}`` to connect to one or more MQTT topics.

.. highlight: django

Example::

    {% wire type={mqtt topic="~site/public/hello"} action={growl text="hello"} %}

.. highlight: erlang

And in Erlang this will trigger the above *growl*::

    z_mqtt:publish(<<"~site/public/hello">>, <<>>, z_acl:sudo(z:c(mysite))).

See also :ref:`scomp-wire`
