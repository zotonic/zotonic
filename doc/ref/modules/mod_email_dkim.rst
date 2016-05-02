
.. include:: meta-mod_email_dkim.rst

Signs outgoing e-mails with DomainKeys Identified Mail Signatures
(`RFC 6376 <https://tools.ietf.org/html/rfc6376>`_).

DKIM (DomainKeys Identified Mail) is an important authentication
mechanism to help protect both email receivers and email senders from
forged and phishing email.

How does it work?
-----------------

DKIM works by signing each e-mail that Zotonic sends with a private
key. The public key part is exposed through a DNS TXT record, with
which email receiver can check whether the email actually originated
from the domain that it claimed to come from.


This RSA keypair is generated automatically when the module is
installed, and the private/public keys are put in the directory
``sitename/dkim/``. When the module is active and the keypair has been
generated, all outgoing e-mail will be signed.

.. note:: The generating of the keypair depends on the ``openssl``
          utility to be available in ``$PATH``.

DNS configuration
-----------------

The receiving e-mail server checks the validity of the signature by
doing a DNS lookup. To configure DKIM, you will need to add this DNS
entry to your domain where you send the mail from.

In the admin, the page ``/admin/email/dkim``, available under
("Modules" / "DKIM e-mail setup") provides information how to
configure this DNS entry, including the text to copy-paste into the
DNS record.


DKIM selector
.............

By default, the DKIM selector is set to the string ``zotonic``. This
will result in DNS lookups to the ``zotonic._domainkey.yoursite.com``
domain. You can change the selector name by adding a config value
called ``site.dkim_selector``.
