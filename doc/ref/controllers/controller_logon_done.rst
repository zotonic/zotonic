
.. include:: meta-logon_done.rst

This controller is used as a jumping stone after a log on from the ``/logon`` page.
The ``p`` argument is passed from the ``/logon`` page.

The controller will notify observers of ``#logon_ready_page{ request_page = P }}`` to
see where to redirect next.

The notification is a `:ref:`notification-first`, so the first module responding
with something else than ``undefined`` will determine the redirect.

If no redirection is returned, and the ``p`` argument is empty, then the user
is redirected to the home page ``/``.


.. seealso:: :ref:`logon_ready_page`, :ref:`controller-authentication`

