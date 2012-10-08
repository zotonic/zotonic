
.. include:: meta-wire_args.rst

Add extra arguments to wired actions.

The tag ``wire_args`` is used to append extra arguments to actions before wiring those actions. This is useful when an action is handed down as an argument to a template but still needs the id of a local element.

For example::

   {% with {my_action some_arg=some_value} as my_action %}
     {% for n in [1,2,3,4] %}
       <a id="{{#id.n}}" href="#">click {{n}}</a>
       {% wire_args action=my_action the_link_id=#id.n %}
     {% endfor %}
   {% endwith %}

This wires the action ``{my_action some_arg=some_value the_link_id=...}`` to the links.

The following arguments are part of the wire tag and can't be used for argument appending: "id", "type", "target", "action", "postback" and "delegate".

.. seealso:: the :ref:`scomp-wire` tag.

