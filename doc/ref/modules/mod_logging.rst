
.. include:: meta-mod_logging.rst

Adds log views to the admin.

Log messages
------------

Any message from Zotonic which uses the ``zInfo`` or ``zWarning``
macros is logged in the log view, in real time.

The log is pruned after three months.


E-mail log
----------

The e-mail log is a separate view, which lists which email messages
have been sent to which recipients. Any mail that gets sent gets
logged here.

The e-mail log is pruned after three months.


User interface log
------------------

Javascript errors are caught in an ``onerror`` handler and sent with a xhr request to
a custom controller in ``mod_logging``. The error is then inserted in the ui-log database
table and in the info lager log.

To prevent spamming of the database table there is a rate limit of 1 error per second.

The database log is also pruned after a week.

.. seealso:: :ref:`controller-log_client`
