
.. include:: meta-api.rst



``controler_api`` Processes authorized REST API requests: It provides
an easy way to create API calls to allow computer programs to perform
functions on your Zotonic site.

``controller_api`` by default intercepts all URLs according to the
patterns ``/api/:module/:method`` and the url ``/api/:module``. On
this URL, a lookup is done to the Zotonic module named `mod_:module`
in its services directory, for a Erlang file called
`service_:module_:method.erl`. If method is left empty (at the
``/api/:module`` URL), the method name equals the module name.

So for example the following lookups result in the following service handlers:

=================  ==========   ========   ====================================
URL                Module       Method     Found service .erl file
=================  ==========   ========   ====================================
/api/base/export   mod_base     export     mod_base/services/service_base_export.erl
/api/base/info     mod_base     info       mod_base/services/service_base_info.erl
/api/search        mod_search   search     mod_search/services/service_search_search.erl
=================  ==========   ========   ====================================


Creating REST services at other URLs
------------------------------------
.. versionadded:: 0.8

It is possible to pre-fill the required `module` and `method`
parameters so that you can use ``controller_api`` at another entry
point. For instance, the following :ref:`dispatch rule <manual-dispatch>` is valid::

    {dosomething,    ["do", "something"], controller_api, [{module, "foobar"}, {method, "test"}]}

This would invoke the ``mod_foobar/services/service_foobar_test.erl`` service at the url `/do/something`,


.. seealso:: :ref:`manual-dispatch` and :ref:`manual-controllers`.
                  
