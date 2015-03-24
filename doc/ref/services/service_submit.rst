
.. include:: meta-submit.rst

Post a survey as JSON.
             
The JSON payload in the body of the request should be a JSON object
with all values from the survey fields. The "id" argument in the URL
specifies which form (survey resource) to submit.

Example::

  curl -vv -H 'Content-type: application/json' -d '{"truefalse1": "no"}' http://localhost:8000/api/survey/submit?id=100

When all is OK, the following result is returned::

  {"result":"submitted"}

In case of an error, an error JSON is returned::

  {"error":{"code":"syntax","message":"Syntax error: Missing fields: email"}}

When an extra query parameter is posted, ``allow_missing=true``, the
API call will never complain about missing fields but just submit the
survey anyway.

