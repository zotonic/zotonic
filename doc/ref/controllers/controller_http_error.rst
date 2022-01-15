
.. include:: meta-http_error.rst

This controller is called for serving http error pages.

The controller will try to match the content type of the
response with the expected content type for the request.

For html responses it will render a page ``error.code.tpl`` 
where *code* is the error code for the response, for example
404 or 500.

In the template the following variables are available:

``error_code``
    The http response error code, an integer like 500.

``error_erlang``
    (Optional) In case of an Erlang error, the textual
    version of the error. Could be a short word or a
    longer descriptive error message.

``error_table``
    (Optional) If an error stack was available then this
    is a list of table rows:
    
    .. code-block:: erlang
    
       [ IsTemplate, Module, Fun, Args, {File, Line} ]

    Where ``Module`` is the template name if ``IsTemplate``
    is ``true``.

``error_dump``
    (Optional) Some raw internal error information. This is
    given if there is an error but the error could not be
    translated into a erlang error and table.

For JSON returns a simple error is returned, for example:

.. code-block:: json

  { "code": 404, "status": "Not Found" }

For image results a transparent 1 pixel gif is served.

For Javascript and css a text file with a comment is served.

For plain text a simple error like ``Not Found`` is served.
