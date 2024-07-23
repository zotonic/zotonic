
.. include:: meta-page.rst

Show a rsc as a HTML page.

This controller is used to show the HTML page of a
:term:`resource`. A “404 Not Found” or “410 Gone” page is shown if the requested
page never existed or has been deleted.

The user will be redirected to the ``logon`` URL when the current user
is not allowed to view the page.

This controller also adds a ``noindex`` response header when the page’s
:ref:`seo_noindex <mod_seo>` flag is set.

Example dispatch rule::

  {page, ["page", id], controller_page, []}


Dispatch arguments
------------------

``controller_page`` recognizes the following arguments inside the dispatch pattern:

+---------------------+-------------------------------------+------------------------+
|Argument             |Description                          |Example URL             |
+=====================+=====================================+========================+
|id                   |The id of the page (rsc) to be       |/page/12345             |
|                     |shown. This can be the numerical id  |                        |
|                     |or the unique name of a page.        |                        |
+---------------------+-------------------------------------+------------------------+


Dispatch options
----------------

The following options can be given to the dispatch rule:

+---------------------+-------------------------------------+------------------------+
|Option               |Description                          |Example                 |
+=====================+=====================================+========================+
|id                   |Id or unique name of the resource to |{id, page_about}        |
|                     |be shown. This overrules any id in   |                        |
|                     |the query arguments. Use ``user_id`` |                        |
|                     |for the id of the current user.      |                        |
+---------------------+-------------------------------------+------------------------+
|template             |Name of the template to be           |{template, "about.tpl"} |
|                     |rendered. Defaults to “page.tpl”     |                        |
|                     |Can also be a tuple of the following |{template, {cat, "home. |
|                     |form: `{cat, Name}`.                 |tpl"}}                  |
|                     |See also: :ref:`tag-catinclude`.     |                        |
+---------------------+-------------------------------------+------------------------+
|cat                  |The category the resource that is    |{cat, text}             |
|                     |requested has to be. If a page of a  |                        |
|                     |different category is requested, a   |                        |
|                     |404 is shown.                        |                        |
+---------------------+-------------------------------------+------------------------+
|acl_action           |What ACL action will be              |{acl_action, edit}      |
|                     |checked. Defaults to 'view'; but can |                        |
|                     |also be 'edit' if users need edit    |                        |
|                     |permission on the rsc to be able to  |                        |
|                     |access the resource.                 |                        |
+---------------------+-------------------------------------+------------------------+
|acl                  |Extra authorization checks to be     |See `ACL options`_.     |
|                     |performed.                           |                        |
+---------------------+-------------------------------------+------------------------+
|is_canonical         |Whether this URL should be considered|{is_canonical, false}   |
|                     |the caninical URL of the page. If so,|                        |
|                     |the controller will redirect to the  |                        |
|                     |rsc's page path if set. Defaults to  |                        |
|                     |true.                                |                        |
+---------------------+-------------------------------------+------------------------+
|seo_noindex          |Ask crawlers to not index this page. |seo_noindex             |
+---------------------+-------------------------------------+------------------------+
|nocache              |Prevent browser caching this page.   |nocache                 |
+---------------------+-------------------------------------+------------------------+

.. _controller-page-acl-options:

.. include:: acl_options.rst

.. seealso:: :ref:`controller-template`.
