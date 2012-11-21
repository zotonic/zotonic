.. _manual-authorization:

Authorization
-------------

Authorization
   The act of defining access policy for a peer.


Authorization is initialized for a request once it has been :ref:`authenticated <manual-authentication>`, by sending a ``#acl_logon{}`` notification. Should the session get logged out, loosing its authentication, the authorization is cleared as well by sending a ``#acl_logoff{}`` notification.

Once authorization has been initialized for a request :term:`context`, operations against objects can be checked by the ``z_acl`` module from erlang code, and by the :ref:`model-acl` model from :ref:`manual-templates`.

The first point of contact for authorization is in the :term:`controller` ``is_authorized/2`` function. :ref:`controller-page` and :ref:`controller-template` checks for ``acl`` options in the :term:`dispatch rule` that matched the current request.

Each controller is in charge of what checks to perform in the ``is_authorized/2`` controller function. Both :ref:`controller-page` and :ref:`controller-template` support the ``acl`` option with a value of:

  - ``is_auth``: request authorized to proceed processing if the session is authenticated.

  - ``logoff``: request authorized to proceed, after removing any authentication from the session.
                Notice: this also clears any "remember me" cookie.    

  - ``[{Action, Object}, ...]``: request authorized to proceed if all operations are allowed.

Refer to the documentation of respective controller for a complete list of options available.

Requested operations are authorized by sending notifications to be checked by a ACL module.

This table lists the ACL notifications:

+----------------------------+----------+----------+---------------------------------------------------------+
|Notification                |Type      |Return    |Description                                              |
+============================+==========+==========+=========================================================+
|#acl_is_allowed{action,     |first     |boolean() |Check if user is authorized to perform operation on      |
|object}                     |          |          |object. Default is ``false``.                            |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_is_allowed_prop{action,|first     |boolean() |Check if user is authorized to perform operation on      |
|object, prop}               |          |          |property of object. Default is ``true``.                 |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_rsc_update_check{id},  |foldl     |Props'    |Filter properties about to be updated for a resource.    |
|Props                       |          |          |                                                         |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_can_see{}              |first     |integer() |Get max :ref:`visible_for <model-rsc>` that the user can |
|                            |          |          |see.                                                     |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_logon{id}              |first     |Context   |Initialize context with the access policy for the        |
|                            |          |          |context.                                                 |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_logoff{}               |first     |Context   |Clear the associated access policy for the context.      |
+----------------------------+----------+----------+---------------------------------------------------------+
