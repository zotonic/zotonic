
.. include:: meta-mod_comment.rst

Implements a basic commenting system, enabling commenting on
:term:`resources <resource>`.

The module has an admin comment overview, allowing the administrator
to review the comments or delete them.

To enable commenting in your site, include ``_comments.tpl`` on your
resource’s page template, passing an `id` parameter for the resource
on which you want users to be able to comment.

It is possible to enable resource rating. When enabled it you can add 
a 1 to 5 star rating to a comment and search resources based on its 
average rating.