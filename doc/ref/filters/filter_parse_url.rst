
.. include:: meta-parse_url.rst

Parses an URL (URI) using ``uri_string:parse/1``.

Example::

   {% print "https://example.com:8443/foo?a=b#test"|parse_url %}

Gives::

    #{fragment => <<"test">>, host => <<"example.com">>,
      path => <<"/foo">>, port => 8443, query => <<"a=b">>,
      scheme => <<"https">>}
