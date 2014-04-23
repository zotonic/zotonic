.. include:: meta-api.rst


``controler_api`` processes authorized REST API requests: It provides
an easy way to create API calls to allow computer programs to perform
functions on your Zotonic site.

``controller_api`` by default intercepts all URLs according to the
patterns ``/api/:module/:method`` and the URL ``/api/:module``. See
the :ref:`manual-services` manual for more information.


Authentication
--------------
See :ref:`manual-services-auth` on how authentication is done when
using this controller.


.. _controller-api-nonstandard-url:

Creating services at a non-standard URL
---------------------------------------
.. versionadded:: 0.8

It is possible to pre-fill the required `module` and `method`
parameters so that you can use ``controller_api`` at another entry
point. For instance, the following :ref:`dispatch rule <manual-dispatch>` is valid::

    {dosomething,    ["do", "something"], controller_api, [{module, "foobar"}, {method, "test"}]}

This would invoke the ``mod_foobar/services/service_foobar_test.erl`` service at the url `/do/something`,


.. seealso:: :ref:`manual-dispatch` and :ref:`manual-controllers`.
                  
