.. highlight:: erlang
.. include:: meta-mod_survey.rst

Adds the concept of `survey` :term:`resources <resource>`:
user-definable forms which can be created in the admin interface and
filled out by the website's visitors.



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
