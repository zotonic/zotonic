.. highlight:: django
.. include:: meta-email_mailinglist_confirm.rst

This template is rendered as an HTML e-mail and gets sent to the
recipient to confirm his subscription to a mailing list.

The ``recipient`` variable holds all information on the recipient of
the mail.

The confirmation URL can be rendered as follows::

  {% url mailinglist_confirm confirm_key=recipient.confirm_key %}

.. seealso:: :ref:`mod_mailinglist`
