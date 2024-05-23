.. include:: meta-api.rst

.. seealso:: :ref:`guide-dispatch` and :ref:`guide-controllers`.

``controler_api`` processes authorized REST API requests: It provides
an easy way to create API calls to allow computer programs to perform
functions on your Zotonic site.

``controller_api`` by default intercepts all URLs according to the
patterns ``/api/:topic``.

The topic can refer to a model. In this case the topic pattern is one of:

 * ``/api/model/mymodel/get/foo/bar`` maps to ``m_mymodel:m_get([ <<"foo">>, <<"bar">> ], Msg, Context)``
 * ``/api/model/mymodel/post/foo/bar`` maps to ``m_mymodel:m_delete([ <<"foo">>, <<"bar">> ], Msg, Context)``
 * ``/api/model/mymodel/delete/foo/bar`` maps to ``m_mymodel:m_delete([ <<"foo">>, <<"bar">> ], Msg, Context)``

In all case the ``Msg`` is an MQTT message map, with the ``payload`` set to the body of the received
request.  In case of a GET or DELETE the payload is set to a map of the query arguments.

Note that for a POST the payload might not be the complete message as additional query arguments are
passed via ``z_context:get_q(<<"argument_name">>, Context)``. If the model function is called using MQTT,
then all arguments are contained in the payload.

The API controller will publish a message to the topic, and wait for max 60 seconds for a response on the
response topic.

If there is a query argument ``response_topic`` then the API controller will only publish the
message and immediately return.
