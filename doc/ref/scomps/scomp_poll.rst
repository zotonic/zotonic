.. highlight:: django
.. include:: meta-poll.rst

Show a given survey (with the ``id`` parameter) as a "poll". This
presents a simpler interface, in which the user is directly asked to
enter some information, e.g. make a choice between certain things::

  {% poll id=123 %}
