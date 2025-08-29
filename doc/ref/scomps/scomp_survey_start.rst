.. highlight:: django
.. include:: meta-survey_start.rst

Show the first page for a given survey (with the ``id`` parameter)::

  {% survey_start id=123 %}

The survey page template will be rendered for the first page and shown
on the place of the scomp. This scomp is useful for surveys that are
configured to start automatically.
