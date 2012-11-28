.. highlight:: django
.. include:: meta-survey_example.rst

Returns a html fragment that can be used as an example for a survey
question. The ``type`` parameter determines which survey question type
to render::
               
  {% survey_example type="likert" %} 

Mainly used in the admin interface when editing surveys.
