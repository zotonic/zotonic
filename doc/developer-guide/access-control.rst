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

Customizing authentication
^^^^^^^^^^^^^^^^^^^^^^^^^^

Zotonic relies on a number of notifications to perform authentication. Observe
any of the authentication notifications to customize Zotonic’s authentication
behaviour. See the reference for a list of all
:ref:`authentication notifications <ref-authentication-notifications>`.

.. _guide-authorization:

Authorization
-------------

Once the request has been authenticated, authorization is initialized by sending
an ``#acl_logon{}`` notification. Should the session get logged out, loosing its
authentication, the authorization is cleared by sending a ``#acl_logoff{}``
notification.

Once authorization has been initialized for a request :term:`context`,
operations against objects can be checked by the ``z_acl`` module from Erlang
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

.. seealso::

    :ref:`mod_acl_user_groups`

Customizing authorization
^^^^^^^^^^^^^^^^^^^^^^^^^

No matter what authorization module you use, you can always override Zotonic’s
behaviour by observing the authorization or ACL notifications. This is
especially useful if your application has some authorization logic that is not
easily expressed in ACL rules. See the reference for a full list of
:ref:`ref-acl-notifications`.
