
.. include:: meta-signup.rst

Exported APIs:

 * ``model/signup/get/confirm_redirect`` return the URL for redirecting the current user after a signup.
    This is done by calling (first) the ``#signup_confirm_redirect{ id = UserId }`` notification. If ``undefined``
    is returned then the URL defaults to the personal page of the user. If no user is logged on then the
    URL of the home page (dispatch rule ``home``) is returned.
