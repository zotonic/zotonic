
.. include:: meta-template.rst

Render a template. When used in a postback action, the result will be
sent back with the response data for the postback.

This is useful when you want to send the output from a template back
as response in a postback or submit event handler.

Example::

   z_render:wire({template, [{template, "my_response.tpl"}, {data,
   Response}]}, Context).

Template accepts the following arguments:

+----------+----------------------------+---------------------------+
|Argument  |Description                 |Example                    |
+----------+----------------------------+---------------------------+
|template  |Name of template to render. |template="my_template.tpl" |
+----------+----------------------------+---------------------------+
|*         |Any other arguments will be |id=123                     |
|          |passed on to the template   |                           |
|          |being rendered.             |                           |
+----------+----------------------------+---------------------------+
