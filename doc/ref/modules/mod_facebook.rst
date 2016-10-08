.. highlight:: django
.. include:: meta-mod_facebook.rst

The mod_facebook module plugs into the
:ref:`authentication systen <guide-authentication>` to enable `Facebook login`_
on your site.

Configuration
-------------

:ref:`Activate <activating-modules>` mod_facebook, then head to ‘Auth’ >
‘External services’ in the admin interface to enter your Facebook app ID and
secret. Enable Facebook login by checking the ‘Use Facebook authentication’ box.
This will add a ‘Log in with Facebook’ button to the logon form on your site.

If you need extended permissions, add them to the ‘Scope’ textbox. Note that the
module needs the ‘email’ permission for login to work.

.. seealso::

    * :ref:`mod_instagram`
    * :ref:`mod_twitter`

.. _Facebook login: https://developers.facebook.com/docs/facebook-login/
