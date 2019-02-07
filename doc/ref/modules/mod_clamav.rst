
.. include:: meta-mod_clamav.rst

Uses ``clamd`` to scan all uploaded files for viruses.

The scanning happens after the mime type and access control is checked, but
before the sanitization. If a file is infected then the error ``infected`` will
be returned. The admin will display a growl message telling that the file
was infected.

Clamd has a maximum size for checks, above that size the error ``sizelimit`` will
be returned.

Configure in the ``zotonic.config`` file where ``clamd`` is listening.

The following configs are available:

``clamav_ip``
    IP address of ``clamd``, default to ``"127.0.0.1"``

``clamav_port``
    Port of ``clamd``, default to ``3310``

``clamav_max_size``
    The **StreamMaxLength** of ``clamd``, default to ``26214400`` (25M)

All clamav results are logged, any infected files or other errors are logged to
the error.log.

Every hour the module checks if it can reach ``clamd`` using the configured settings.
It will log an error if ``clamd`` can’t be reached, and an info message if it can be reached.
