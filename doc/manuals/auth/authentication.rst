.. _manual-authentication:

Authentication
--------------

Authentication
  The act of establishing a confirmed identity of our peer.


Authentication is done for every request that is processed by zotonic. Using cookies, this process is simplified by returning a :term:`session cookie` along with the response to the client once authenticated. For subsequent requests, the request can be authenticated by the current session when the session cookie is sent along with the request.

Authenticating a request with a session cookie does not take place until a session is ensured (or continued) for the request :term:`context`. This is commonly done by the :term:`controller` handling the request by a call to ``z_context:ensure_all/1``.

Requests that does not yet have a session cookie, or whose session cookie has expired, need to provide some credentials in order to be authenticated. The :ref:`logon controller <controller-logon>` takes care of processing login requests and checks for the presence of a "remember me" cookie for automatic login. 

Zotonic relies on a number of :ref:`notifications <manual-notification>` to support and manage the authentication functionality. An overview of the notifications involved during authentication is given in the following table:

+--------------------+----------+----------+------------------------------------------+
|Notification        |Type      |Return    |Description                               |
+====================+==========+==========+==========================================+
|auth_confirm        |foldl     |Context   |Sent when a user id has been confirmed.   |
|                    |          |          |                                          |
|                    |          |          |                                          |
|                    |          |          |                                          |
+--------------------+----------+----------+                                          |
|auth_confirm_done   |notify    |          |                                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logon          |foldl     |Context   |Sent when a user has been authenticated.  |
|                    |          |          |                                          |
|                    |          |          |                                          |
+--------------------+----------+----------+                                          |
|auth_logon_done     |notify    |          |                                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logoff         |foldl     |Context   |Sent when a user is about to log out,     |
|                    |          |          |removing the authentication from the      |
|                    |          |          |current session.                          |
|                    |          |          |                                          |
|                    |          |          |                                          |
|                    |          |          |                                          |
|                    |          |          |                                          |
|                    |          |          |                                          |
+--------------------+----------+----------+------------------------------------------+
|auth_logoff_done    |notify    |          |Sent when a user has been logged out.     |
|                    |          |          |                                          |
|                    |          |          |                                          |
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
|                    |          |          |                                          |
+--------------------+----------+----------+------------------------------------------+
