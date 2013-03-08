
.. include:: meta-template.rst

Show a template.

This controller renders the template configured in the dispatch rules.

Example dispatch rule::

  {home, [], controller_template, [{template, "home.tpl"}]}

This will render the ``home.tpl`` template at the url ``/``.


Dispatch arguments
------------------

``controller_template`` recognizes the following arguments inside the dispatch pattern:

+---------------------+---------------------------------------+------------------------+
|Argument             |Description                            |Example URL             |
+=====================+=======================================+========================+
|id                   |A resource id to be used in the        |/page/12345             |
|                     |template. This can be the numerical id |                        |
|                     |or the unique name of a page. More     |                        |
|                     |commonly the id is given as a dispatch |                        |
|                     |option.                                |                        |
+---------------------+---------------------------------------+------------------------+


Dispatch options
----------------

The following options can be given to the dispatch rule:

+---------------------+--------------------------------------+------------------------+
|Option               |Description                           |Example                 |
+=====================+======================================+========================+
|template             |Name of the template to be rendered.  |{template, "home.tpl"}  |
+---------------------+--------------------------------------+------------------------+
|anonymous            |Render the template always as the     |{anonymous, true}       |
|                     |anonymous user, even when an user is  |                        |
|                     |logged on. Defaults to false.         |                        |
+---------------------+--------------------------------------+------------------------+
|content_type         |The content type provided by the      |{content_type,          |
|                     |dispatch rule. Defaults to            |"application/json"}     |
|                     |“text/html”.                          |                        |
+---------------------+--------------------------------------+------------------------+
|maxage               |The number of seconds of how long to  |{maxage, 3600}          |
|                     |cache this file in the browser. Sets  |                        |
|                     |the response header: `Cache-control:  |                        |
|                     |public; maxage=X`.                    |                        |
+---------------------+--------------------------------------+------------------------+
|acl_action           |What ACL action will be               |{acl_action, edit}      |
|                     |checked. Defaults to 'view'; but can  |                        |
|                     |also be 'edit' if users need edit     |                        |
|                     |permission on the rsc to be able to   |                        |
|                     |access the resource.                  |                        |
+---------------------+--------------------------------------+------------------------+
|acl                  |Extra authorization checks to be      |See `ACL options`_.     |
|                     |performed.                            |                        |
+---------------------+--------------------------------------+------------------------+
|id                   |Id or unique name of a resource to be |{id, page_about}        |
|                     |referenced in the rendered            |                        |
|                     |template. This overrules and id from  |                        |
|                     |the query arguments.                  |                        |
+---------------------+--------------------------------------+------------------------+


.. include:: acl_options.rst

.. seealso:: :ref:`controller-page`.
