
.. highlight:: django
.. include:: meta-acceptable_password.rst
.. seealso:: :ref:`guide-validators`, :ref:`validator-username_unique`

A :ref:`validator <guide-validators>` to check whether a password conforms to the
password secutiry requirements.

It can be attached to a password entry field used to set a new password::

    <input type="password" id="password" name="password" autocomplete="new-password" value="">
    {% validate id="password"
                type={acceptable_password}
                only_on_blur
    %}

The password will be sent to the server when the user leaves the password field.

The server will then check for:

 * minimum password length, as configured in ``mod_authentication.password_min_length`` (defaults to 8)
 * matching a regular expression with criteria, as configured in ``mod_authentication.password_min_length``
 * check if the password does not appear in a leak, unless ``mod_authentication.password_disable_leak_check`` is set (this uses the service at `Have I Been Pwned <https://haveibeenpwned.com/Passwords>`_)

You can pass a ``failure_message``::

    <input type="password" id="password" name="password" autocomplete="new-password" value="">
    {% validate id="password"
                type={acceptable_password
                    failure_message=_"Your new password is too short or not strong enough"
                }
                only_on_blur
    %}

There is an optional parameter ``allow_empty`` to allow empty passwords. This is useful if the
password should only be filled in special circumstances (like when changing the password) and
the handler code knows not to do anything if the password is empty::

    <label for="password">{_ Optionally set a new password _}</label>
    <input type="password" id="password" name="password" autocomplete="new-password" value="">
    {% validate id="password"
                type={acceptable_password
                    allow_empty
                    failure_message=_"Your new password is too short or not strong enough"
                }
                only_on_blur
    %}


