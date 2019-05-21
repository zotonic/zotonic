
.. include:: meta-mod_logging.rst

Adds log views to the admin.

Log messages
------------

Any message from Zotonic which uses the ``zInfo`` or ``zWarning``
macros is logged in the log view, in real time.


E-mail log
----------

The e-mail log is a separate view, which lists which email messages
have been sent to which recipients. Any mail that gets sent gets
logged here.


User interface log
------------------

The user interface log shows Javascript errors that happened on the clients.
At most one error per second is logged, others are dropped.


.. todo:: Add more documentation
