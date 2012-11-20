
.. include:: meta-logon.rst

This controller logs on a user, and optionally sets a "remember me"
cookie.

When ``p`` argument is given, the user is redirect to the page given.

The controller also has postback ``event/2`` calls for the following
interactions:

- Login confirmation
- Password reset
- Send password reminder  

.. todo:: Extend documentation

.. seealso:: :ref:`controller-logoff`, :ref:`manual-authentication`.
