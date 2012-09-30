
.. include:: meta-req.rst
.. highlight:: django

This model gives access to the request variables from within a
template.

Sometimes you need direct access to request variables in your
template.  The m_req model is meant for this.  It exposes some values
from the Webmachine request record.


Fetching a single value
.......................

You can fetch individual values by key, for example::

  {{ m.req.host|escape }}

Viewing all request variables
.............................

Use the :ref:`tag-print` tag to get a complete overview of all request
variables::

  {% print m.req %}

This will show something like::

  {version,{1,1}},
  {peer,"127.0.0.1"},
  {is_ssl,false},
  {host,"127.0.0.1:8000"},
  {raw_path,"/?foo=bar"},
  {path,"/"},
  {qs,[{"foo","bar"}]},
  {headers,[{"accept",
            "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"},
           {"accept-encoding","gzip, deflate"},
           {"accept-language","en-us"},
           {"connection","keep-alive"},
           {"cookie","z_sid=1RrDIvFVsoyLgtSU62mN; z_pid=HRrAaiPI08Il4Nu4wST1"},
           {"host","127.0.0.1:8000"},
           {"referer","http://localhost:8000/"},
           {"user-agent",
            "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_4; en-us) AppleWebKit/533.16 (KHTML, like Gecko) Version/5.0 Safari/533.16"}]}]

Please note that all values are raw and not escaped, take care to
escape the values before you use them in your templates, using the
:ref:`filter-escape` filter.

