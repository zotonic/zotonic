
.. include:: meta-mod_admin_identity.rst

Provides "identity management" in the admin - for example the storage of usernames and passwords.

Password Complexity Rules
-------------------------

By default Zotonic only enforces that your password is not
blank. However, if you, your clients or your business require the
enforcement of Password Complexity Rules Zotonic provides it.

Configuring Password Rules
..........................

After logging into the administration interface of your site, go to
the `Config` section and click `Make a new config setting`.  In the
dialog box, enter ``mod_admin_identity`` for Module, ``password_regex`` for Key and
your password rule regular expression for Value.

After saving, your password complexity rule will now be enforced on
all future password changes.

Advice on Building a Password Regular Expression
................................................

A typical password_regex should start with ^.* and end with .*$. This allows everything by default and allows you to assert typical password rules like:

- must be at least 8 characters long (?=.{8,})
- must have at least one number (?=.*[0-9])
- must have at least one lower-case letter (?=.*[a-z])
- must have at least one upper-case letter (?=.*[A-Z])
- must have at least one special character (?=.*[@#$%^&+=])
    
Putting those rules all together gives the following password_regex::

  ^.*(?=.{8,})(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%^&+=]).*$
  
To understand the mechanics behind this regular expression see
`Password validation with regular expressions
<http://www.zorched.net/2009/05/08/password-strength-validation-with-regular-expressions/>`_.

