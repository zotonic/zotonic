
.. include:: meta-identity.rst



The m_identity model manages usernames and other user
identities. :ref:`mod_authentication` uses it to store and check
salted passwords, but also provides a safe storage for user tokens of
any kind, as used by :ref:`mod_facebook` and :ref:`mod_twitter`.

Note that a `user` does not have to be of the `person` category per
se, in Zotonic anything can have identities attached to it.

The following m_identity model properties are available in templates:

+--------+----------------------------+-------------+
|Property|Description                 |Example value|
+========+============================+=============+
|is_user |Check if a page id is an    |true         |
|        |user. Return a bool. Usage: |             |
|        |m.identity[page_id].is_user |             |
+--------+----------------------------+-------------+
|username|Fetch the username, if any, |<<"admin">>  |
|        |of an user. Returns a binary|             |
|        |or undefined. Usage:        |             |
|        |m.identity[page_id].username|             |
+--------+----------------------------+-------------+

.. seealso:: :ref:`manual-auth`, :ref:`mod_authentication`,
             :ref:`mod_twitter` or :ref:`mod_facebook`.
