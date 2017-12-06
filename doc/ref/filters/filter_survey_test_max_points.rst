.. highlight:: django
.. include:: meta-survey_test_max_points.rst

Counts the total of all points that can be received for all *test* questions.
Non *test* questions are not counted.

Usage::

    {{ survey_id|survey_test_max_points }}

Return the number of points if all questions are corectly answered.

See :ref:`mod_survey`
