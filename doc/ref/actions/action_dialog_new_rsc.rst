
.. include:: meta-dialog_new_rsc.rst

Show the admin dialog for creating a new :term:`resource`.

When the resource is created, the user is redirected to the admin edit
page. This action exports a postback called ``new_page`` which is used
to create the page::

  {% wire id=#form type="submit" 
	postback={new_page subject_id=subject_id predicate=predicate redirect=redirect 
			  actions=actions callback=callback}
	delegate=`action_admin_dialog_new_rsc`
  %}


This postback has the following arguments:

* ``subject_id`` + ``predicate``: Create an edge from the given
  subject to this new page, using the given predicate.
* ``redirect``: Boolean flag whether or not to redirect to the edit
  page. Defaults to ``true``.
* ``actions``: Any actions to perform after the resource is created.
* ``callback``: Javascript function to call when the subject edge has been created.
* ``objects``: A list of ``[object, predicate]`` pairs which are
  created as outgoing edges from the new page to the given
  objects. The object can be a resource ID or a resource
  name. Example::

    objects=[ [m.acl.user, "author"] ]

  creates an "author" edge from the new page to the currently logged in user.

.. todo:: Extend documentation
