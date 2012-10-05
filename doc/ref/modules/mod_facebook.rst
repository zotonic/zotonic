.. highlight:: django
.. include:: meta-mod_facebook.rst

Plugs into the authentication / signup system to enable Facebook
login.

Module is still largely undocumented, but this is what was said on the
mailing list about it:

The url of your site needs to be correctly configured in the Facebook app configuratoin at Facebook for your site to work.

The current (hard coded) test site has the url http://127.0.0.1:8000/ configured.
Use the config keys:

- ``mod_facebook.appid``
- ``mod_facebook.appsecret``
    
Both values you can find when you register your site (as an application) at Facebook.
    
If you need extended permission for the acquired token, you can use
the ``mod_facebook.scope`` configuration key to set the scope. Note
that the module always will need the "email" permission for the login
to work.

.. todo:: Unfinished doc
