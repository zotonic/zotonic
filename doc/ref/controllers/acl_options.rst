ACL options
-----------

:ref:`manual-authorization` checks to perform, in addition to the
``acl_action`` dispatch option, can be given in the ``acl`` dispatch
option, and accepts the following options:

+----------------------+---------------------------------------------+-----------------------------+
|ACL option            |Description                                  |Example                      |
+======================+=============================================+=============================+
|is_auth               |Disable anonymous access to this resource.   |{acl, is_auth}               |
+----------------------+---------------------------------------------+-----------------------------+
|logoff                |Log out user before processing the request.  |{acl, logoff}                |
+----------------------+---------------------------------------------+-----------------------------+
|{Action, Resource}    |Check if user is allowed to perform `Action` |{acl, {edit, my_named_page}} |
|                      |on `Resource`. The example is equivalent to  |                             |
|                      |the options ``{acl_action, edit}, {id,       |                             |
|                      |my_named_page}``.                            |                             |
+----------------------+---------------------------------------------+-----------------------------+
|[{Action, Resource}]  |A list of checks to be performed, as above.  |{acl, [{view, secret_page},  |
|                      |                                             |{update, 345}]}              |
|                      |                                             |                             |
|                      |                                             |                             |
+----------------------+---------------------------------------------+-----------------------------+
|ignore                |Don’t peform any access control checks.      |{acl, ignore}                |
|                      |Be careful to add your own checks in the     |                             |
|                      |rendered template and all its included       |                             |
|                      |templates.                                   |                             |
+----------------------+---------------------------------------------+-----------------------------+
