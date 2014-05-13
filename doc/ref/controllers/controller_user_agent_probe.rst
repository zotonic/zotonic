
.. include:: meta-user_agent_probe.rst

Controller which serves a small Javascript file which is use to
determine the user agent of the browser, for properties which cannot
be determined from the User Agent header, like the screen size and
(multi)touch-capability.

The Javascript also communicates the timezone back to the server.
This timezone is then stored in the persistent cookie and the session.

.. todo:: Extend documentation
