
.. include:: meta-email_dkim.rst

DomainKeys Identified Mail Signatures (RFC 6376) is a method to add a signature to outgoing emails.
This enables recipients to check who sent an email and if the email was not changed in transit.

This module generates an RSA keypair which will be used when signing outgoing emails.

The keys are stored in the security directory of Zotonic.

If this module is activated then a panel with information is added to the System > Email configuration
menu. The panel includes information about the DKIM key that needs to be added to the DNS.

