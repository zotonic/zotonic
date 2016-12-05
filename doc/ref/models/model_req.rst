
.. include:: meta-req.rst
.. highlight:: django

This model gives access to the request variables from within a
template.

Sometimes you need direct access to request variables in your
template.  The m_req model is meant for this.  It exposes some values
from the Cowmachine request record.


Fetching a single value
.......................

You can fetch individual values by key, for example::

  {{ m.req.host|escape }}

Viewing all request variables
.............................

Use the :ref:`tag-print` tag to get a complete overview of all request variables::

  {% print m.req|make_list %}

This will show something like::

  [{method,<<"GET">>},
   {version,{1,1}},
   {peer,<<"127.0.0.1">>},
   {is_ssl,false},
   {host,<<"test.dev">>},
   {raw_path,<<"/en/page/1234?foo=bar">>},
   {path,<<"/en/page/1234">>},
   {qs,[{<<"foo">>,<<"bar">>}]},
   {referrer,<<"http://test.dev:8000/">>},
   {user_agent,<<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/601.4.4 (KHTML, like Gecko) Version/9.0.3 Safari/601.4.4">>},
   {is_crawler,false},
   {req_id,525158920},
   {headers,[{<<"accept">>,
              <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>},
             {<<"accept-encoding">>,<<"gzip, deflate">>},
             {<<"accept-language">>,<<"en-us">>},
             {<<"cache-control">>,<<"max-age=0">>},
             {<<"connection">>,<<"keep-alive">>},
             {<<"cookie">>,
              "z_logon=; z_sid=LopWHBmHXCs94virnboZhBHLKV6m1Cga; z_ua=c%3Ddesktop%26u%3D1%26t%3D1%26w%3D1920%26h%3D1200"},
             {<<"dnt">>,<<"1">>},
             {<<"host">>,<<"test.dev:8000"},
             {<<"referer">>,<<"http://test.dev:8000/">>},
             {<<"user-agent">>,
              <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/601.4.4 (KHTML, like Gecko) Version/9.0.3 Safari/601.4.4">>}]},
   {timezone,<<"UTC">>},
   {language,en}]


Please note that all values are raw and not escaped, take care to
escape the values before you use them in your templates, using the
:ref:`filter-escape` filter.

The :ref:`filter-make_list` filter is used to force the evaluation of the
model; otherwise it would just print ``{m,req,undefined}``.
