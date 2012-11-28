
.. include:: meta-mod_signup.rst

This module presents an interface for letting users register
themselves.

Notifications
-------------

``identify_verification{user_id=UserId, identity=Ident}``

  Sent verification requests to non verified identities.

``signup_check``

  Fold for the signup preflight check. Allows to add extra user properties or abort the signup.

  If no ``{ok, _Props1, SignupProps}`` is returned, but ``{error,
  Reason}``, the signup is aborted.

``signup_done{id=Id, is_verified=IsVerified, props=Props, signup_props=SignupProps}``
  Fired when a signup procedure is done and a user has been created.

``signup_confirm{id=UserId}``
  Fired when a user has signed up and confirmed his identity (e.g. over email)

``signup_confirm_redirect{id=UserId}``
  Decide to which page a user gets redirected to after signup

.. todo:: Add more documentation
