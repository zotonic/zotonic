.. _rel-0.44.0:

Release 0.44.0
==============

Welcome to Zotonic 0.44.0, released on 18 January, 2019.

Main changes are:

 * New module ``mod_ratelimit``, which limits authentication requests and password resets
 * Automatic session timeout, configure with ``site.session_expire_inactive`` (default 14400 seconds)
 * See all your active sessions at ``/logon/sessions``
 * Mnesia files are now placed in a directory per node, e.g. ``priv/mnesia/zotonic001@foobar/``

BC breaks
---------

* The password reset dispatch rule was changed. If you override ``email_password_reset.tpl``,
  you now need to pass the username too.

  Before::

    {% url logon_reset secret=secret use_absolute_url %}

  After::

    {% url logon_reset secret=secret u=username use_absolute_url %}

 * The delegate and postbacks for authentication requests has been changed.
   Use now `controller_logon` and check the logon and password reset templates for the
   correct arguments. If you didn't add your own authentication templates then you
   don't need to do anything.

Commits since 0.43.0
--------------------

David de Boer (1):

    * core: Remove pivot_resource/2 export (#1967)

Maas-Maarten Zeeman (2):

    * Prevent a crashing mqtt module subscriber to also crash the module. Fixes #1962
    * Added the possibility to compile when running zotonic (#1954)

Marc Worrell (17):

    * mod_twitter: fix coordinates lookup.
    * mod_email_status: fix problem upgrading older email_status module.
    * mod_twitter: fix a problem with the first page when following users.
    * core: also trigger pivot_done on manual rsc pivot (#1966)
    * Pivot done (#1968)
    * core: Move mnesia files to a directory per node. (#1969)
    * mod_facebook: Use https for the Facebook share link
    * Ratelimit for authentication requests (#1970)
    * Session timeout on inactivity and session overview (#1972)
    * mod_base: make the z_logon cookie more secure.
    * core: fix problem with setting x-frame-options
    * Add 'Please try again in ...' to the password reset forms.
    * mod_authentication: add nl translations.
    * mod_authentication: pass wire-args on logon.
    * mod_seo: use Google Analytics localStorage instead of cookies.
    * Remove compile warnings.
    * Upgrade webzmachine
