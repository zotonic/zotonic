.. highlight:: erlang
.. include:: meta-mod_survey.rst

Adds the concept of `survey` :term:`resources <resource>`:
user-definable forms which can be created in the admin interface and
filled out by the website’s visitors.


Survey question types
---------------------

The following question types are defined in the survey.

likert
  Answer a question on a scale of 5 points, from "completely
  disagree" (1) to "completely agree" (5).

long answer
  An open question with a big text field.

matching
  Question type which allows you to match given answers to each other.

narrative
  Question type for specifying inline questions in a narrative fashion.

page break
  Breaks the survey into multiple pages.

short answer
  An open question with a single-lined text field. You have the option of specifying a validation like email, date, numeric.

thurstone
  A multiple choice field. Like multiple choice, but more powerful. The choices are translatable, and you have the possibility to select either a single answer, multiple answers or submit the form directly when choosing an answer.

true or false
  Answers a true or false question. You have the option to specify
  custom texts for both the options.

yes or no
  Like true or false, answers a true or false question. You have the
  option to specify custom texts for both the options.

multiple choice
  A simple multiple choice field that has the added option that the
  multiple choice can be a numeric value, in which case an overview of the
  total value will be shown in the printable list and beneath the
  survey pie chart. This is useful for creating forms which require
  you to enter an amount or quantity, e.g. for a reservation
  system. Multiple choice fields cannot currently be translated, use the "thurstone" question type in that case.

category
  Choose a single resource from a given category as the answer to this question.

subhead
  Renders a sub-heading between questions.

prompt
  Renders an extra prompt block.

text block
  Renders a text block between questions.



Intercepting survey submissions
-------------------------------

When a survey is submitted, the survey module sends out a
``#survey_submit{}`` notification.

This notification has the following fields:

* id - The id of survey being submitted
* handler - A handler name (see below)
* answers - The answers that were filled in
* missing - answers that were missing
* answers_raw - Unprocessed answers, e.g. the raw submission

To intercept a survey submission you would observe this survey_submit
notification, and return ``ok``::

  observe_survey_submit(#survey_submit{id=SurveyId}, Context) ->
      ?DEBUG(SurveyId),
      ok.

      
Creating a custom survey handler
--------------------------------

The survey edit page has a dropdown for so-called "survey handlers". A
survey handler is a property that is set on the resource that
indicates the handler that needs to be taken. Handlers are collected
using the ``#survey_get_handlers{}`` `fold` notification.

For instance, the following defines a handler called "email_me"::

  observe_survey_get_handlers(#survey_get_handlers{}, All, Context) ->
    [
     {<<"email_me">>, "E-mail me when survey is submitted"}
     | All
    ].

Each handler will show up in the dropdown list and the editor can pick
which handler he wants. The value chosen is passed along in the
``handler`` property of the survey submission, and as such can be used
to intercept the survey submission::
    
  observe_survey_submit(#survey_submit{handler= <<"email_me">>, id=SurveyId}, Context) ->
      %% Do something here for surveys which have 'email_me' selected as handler
      ok;
  observe_survey_submit(#survey_submit{}, _Context) ->
      %% Let other surveys use the default submision mechanism
      undefined.



.. todo:: Add more documentation
