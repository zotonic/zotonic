.. _https-support:

HTTPS support
=============

Zotonic has built-in support for HTTPS and TLS (previously SSL) certificate
handling.

All sites share a single TLS port using Server Name Indication for
virtual hosting and certificate selection.

The following guides are useful:

:ref:`guide-deployment-privilegedports`
    Let connect to the default https (443) and http (80) ports.

:ref:`ref-port-ssl-configuration`
    Configure all Zotonic listen ports, depending on your server configuration
    and proxy usage.

:ref:`guide-deployment-nginx`
    If you want to use nginx as a terminating proxy for SSL connections.

:ref:`mod_ssl_letsencrypt`
    Automatically fetch free SSL certificates from Let’s Encrypt.

:ref:`mod_ssl_ca`
    Use SSL certificates obtained from a Certificate Authority.
