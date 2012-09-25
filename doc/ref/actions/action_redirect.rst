
.. include:: meta-redirect.rst


This action redirects the browser to another page or back to the previous page.

Example::

   {% button title="home" action={redirect location="/"} %}

Redirects back to the home page when the button is clicked.

Back in history example::

   {% button title="Back" action={redirect back} %}

After clicking the button the browser will go back to the last page using the Javascript history.

Example of using dispatch rules for the redirect location::

   {% button title="edit" action={redirect dispatch="admin_edit_rsc" id=my_id} %}

When clicked the browser is redirected to the admin edit page for the :term:`resource` with the id of `my_id`.

This action can have the following arguments:

========  =============================================================  ============
Argument  Description                                                    Example
========  =============================================================  ============
back      When given then the browser is directed to the previous page.  back
dispatch  The name of a dispatch rule.  All other parameters are 
          assumed to be parameters for the dispatch rule.                dispatch="admin"
id        When back and dispatch are not defined then the redirect uri 
          will be the page_url of the resource.                          id=42
location  The http address to redirect to.
          Can be an url with or without host name.                       location="http://example.com"
========  =============================================================  ============
