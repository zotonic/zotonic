
.. include:: meta-mod_email_relay.rst

Enables the Zotonic site to `relay` emails for the site’s users to
their real email addresses.
             
The user’s email address is `username@hostname`, where the hostname is
the hostname as configured in the :ref:`sites`’s config file
<manual-site-anatomy>`.  Any mails to those addresses get forwarded to
the user’s email address, as configured in the user :term:`resource`.

Any email that has no valid recipient is rejected.

.. seealso:: :ref:`mod_email_receive`, :ref:`manual-email`.
   
.. todo:: Add more documentation
