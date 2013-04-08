
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


Migrating from an older password hashing scheme
...............................................

When you migrate a legacy system to Zotonic, you might not want your
users to re-enter their password before they can log in again.

By implementing the ``identity_password_match`` notification, you can
have your legacy passwords stored in a custom hashed format, and
notify the system that it needs to re-hash the password to the
Zotonic-native format. The notification has the following fields::

  -record(identity_password_match, {rsc_id, password, hash}).

Your migration script might have set the ``username_pw`` identity with
a marker tuple which contains a password in MD5 format::
  
  m_identity:set_by_type(AccountId, username_pw, Email, {hash, md5, MD5Password}, Context),

Now, in Zotonic when you want users to log on using this MD5 stored
password, you implement ``identity_password_match`` and do the md5
check like this::
  
  observe_identity_password_match(#identity_password_match{password=Password, hash={hash, md5, Hash}}, _Context) ->
    case binary_to_list(erlang:md5(Password)) =:= z_utils:hex_decode(Hash) of
        true ->
            {ok, rehash};
        false ->
            {error, password}
    end;
  observe_identity_password_match(#identity_password_match{}, _Context) ->
    undefined. %% fall through

This checks the password against the old MD5 format. The ``{ok,
rehash}`` return value indicates that the user's password hash will be
updated by Zotonic, and as such, this method is only called once per
user, as the next time the password is stored using Zotonic's internal
hashing scheme.
