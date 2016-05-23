.. _guide-auth:

Access control
==============

Access control is about defining who is allowed to access certain resources.
It takes two steps:

1. The user is *authenticated*, that is, the user’s identity is determined based
   on some form of identification in the request, such as a session cookie.

2. After the user has been identified, we can decide whether that user is
   *authorized* to access a certain URL or resource.

.. _guide-authentication:

Authentication
--------------

For each HTTP request that Zotonic receives, it looks for some form of
credentials that can identity the user. This can be a username/password
combination when the user logs in for the first time, and a
:term:`session cookie` for subsequent requests. When Zotonic finds some
credentials, it checks them for validity. If the credentials are valid, the user
is said to be authenticated and authorization can start.

The first request, that does not yet have a session cookie or whose session
cookie has expired, needs to contain some credentials in order to be
authenticated. The :ref:`logon controller <controller-logon>` takes care of
processing login requests and checks for the presence of a ‘remember me’ cookie
for automatic login. It then responds with a fresh session cookie that the
client will send along with subsequent requests.

Authenticating subsequent requests, that have a session cookie, does not take
place until a session is ensured (or continued) for the request :term:`context`.
This is commonly done by the :term:`controller` handling the request by a call
to ``z_context:ensure_all/1``.

Notifications
^^^^^^^^^^^^^

Zotonic relies on a number of :ref:`notifications <guide-notification>` to
perform authentication. You can observe any of these notifications to customise
Zotonic’s authentication behaviour.

+--------------------+----------+----------+------------------------------------------+
|Notification        |Type      |Return    |Description                               |
+====================+==========+==========+==========================================+
|auth_confirm        |foldl     |Context   |Sent when a user id has been confirmed.   |
+--------------------+----------+----------+                                          |
|auth_confirm_done   |notify    |          |                                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logon          |foldl     |Context   |Sent when a user has been authenticated.  |
+--------------------+----------+----------+                                          |
|auth_logon_done     |notify    |          |                                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logoff         |foldl     |Context   |Sent when a user is about to log out,     |
|                    |          |          |removing the authentication from the      |
|                    |          |          |current session.                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logoff_done    |notify    |          |Sent when a user has been logged out.     |
+--------------------+----------+----------+------------------------------------------+
|auth_autologon      |first     |{ok,      |Sent for new sessions from                |
|                    |          |UserId}   |``z_auth:logon_from_session/1``. Will     |
|                    |          |          |attempt to authenticate the session as    |
|                    |          |          |UserId (if there was any observer         |
|                    |          |          |responding to the notification).          |
+--------------------+----------+----------+------------------------------------------+
|#user_is_enabled{id}|first     |boolean() |Ask observers if the user is enabled      |
|                    |          |          |(allowed to login, to be                  |
|                    |          |          |authenticated). If the result is          |
|                    |          |          |``undefined``, the resource               |
|                    |          |          |``is_published``, ``publication_start``   |
|                    |          |          |and ``publication_end`` is checked        |
|                    |          |          |instead.                                  |
+--------------------+----------+----------+------------------------------------------+

.. _guide-authorization:

Authorization
-------------

Once the request has been authenticated, authorization is initialized by sending
an ``#acl_logon{}`` notification. Should the session get logged out, loosing its
authentication, the authorization is cleared by sending a ``#acl_logoff{}``
notification.

Once authorization has been initialized for a request :term:`context`,
operations against objects can be checked by the ``z_acl`` module from erlang
code, and by the :ref:`model-acl` model from :ref:`templates <guide-templates>`.

Protecting access to controllers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first point of contact for authorization is in the :term:`controller`’s
``is_authorized/2`` function. Both :ref:`controller-page` and
:ref:`controller-template` check for :ref:`controller-page-acl-options` in the
:term:`dispatch rule` that matched the current request.

.. seealso:: :ref:`controller-page-acl-options`

Protecting access to resources and modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Zotonic ships with :ref:`mod_acl_user_groups`, a powerful user group-based
authorization module. With this module you can define access control rules that
determine which user groups are allowed to access which groups of content.

If your ACL needs are simple, you may want to use :ref:`mod_acl_simple_roles`
instead.

.. seealso::

    :ref:`mod_acl_user_groups`, :ref:`mod_acl_simple_roles`

Custom authorization
^^^^^^^^^^^^^^^^^^^^

No matter what authorization module you use, you can always override Zotonic’s
behaviour by observing the authorization notifications. This is especially
useful if your application has some authorization logic that is not easily
expressed in ACL rules.

.. seealso:: :ref:`acl_is_allowed`

Notifications
^^^^^^^^^^^^^

The authorization system sends several notifications that you can hook into to
allow or deny user access to specific resources.

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
|#acl_logon{id}              |first     |Context   |Initialize context with the access policy for the user.  |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_logoff{}               |first     |Context   |Clear the associated access policy for the context.      |
+----------------------------+----------+----------+---------------------------------------------------------+
|#acl_context_authenticated{}|first     |Context   |Set the context to a typical user’s permissions, do not  |
|                            |          |          |change the context if an user is logged on. Used by      |
|                            |          |          |(for example) ``m.acl.authenticated.insert.article``     |
+----------------------------+----------+----------+---------------------------------------------------------+
