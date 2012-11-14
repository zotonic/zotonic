
.. index:: tag; lib
.. _tag-lib:

lib
===

Combine css and javascript includes in a single request.

Generates a ``<link />`` or ``<script />`` element for including the
given libraries. This combines all css and javascript files in one
request, saving multiple roundtrips to the server.

Example::

  {% lib 
	"css/zp-compressed.css"
	"css/zp-admin.css"
	"css/zp-wysiwyg.css"
	"css/zp-dialog.css"
	"css/zp-formreplace.css"
	"css/zp-finder.css"
	"css/zp-growl.css"
    %}

Will output::

  <link href="/lib/css/zp-compressed~zp-admin~zp-wysiwyg~zp-dialog~zp-formreplace~zp-finder~zp-growl~63417066183.css" type="text/css" media="all" rel="stylesheet" />

The number at the end is the Unix modification time of the most recently changed file.

The `lib` tag supports optional arguments to control the resulting html tag:

+-----------------+-------------+---------------------------------------------------------+
|Option           |Default      |Description                                              |
+=================+=============+=========================================================+
|use_absolute_url |false        |If true, prefix the generated URL with                   |
|                 |             |"http://{hostname}/".                                    |
+-----------------+-------------+---------------------------------------------------------+
|title            |<empty>      |Specify a value for the title attribute of the link tag. |
+-----------------+-------------+---------------------------------------------------------+
|media            |"all"        |Specify value for the media attribute of the link tag.   |
+-----------------+-------------+---------------------------------------------------------+
|rel              |"stylesheet" |Specify value for the rel attribute of the link tag.     |
+-----------------+-------------+---------------------------------------------------------+

.. seealso:: :ref:`mod_development`.
