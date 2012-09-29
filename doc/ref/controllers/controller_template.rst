
.. include:: meta-template.rst

Show a template.

This controller renders the template configured in the dispatch rules.

Example dispatch rule::

  {home, [], resource_template, [{template, "home.tpl"}]}

This will render the ``home.tpl`` template at the url ``/``.

Dispatch options
----------------
The following options can be given to the dispatch rule:

+---------------------+-------------------------------------+------------------------+
|Option               |Description                          |Example URL             |
+---------------------+-------------------------------------+------------------------+
|template             |Name of the template to be rendered. |{template, "home.tpl"}  |
+---------------------+-------------------------------------+------------------------+
|anonymous            |Render the template always as the    |{anonymous, true}       |
|                     |anonymous user, even when an user is |                        |
|                     |logged on. Defaults to false.        |                        |
+---------------------+-------------------------------------+------------------------+
|content_type         |The content type provided by the     |{content_type,          |
|                     |dispatch rule. Defaults to           |"application/json"}     |
|                     |“text/html”.                         |                        |
+---------------------+-------------------------------------+------------------------+

This resource does not handle any request arguments.
