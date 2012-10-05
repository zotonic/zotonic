.. _manual-email:

E-mail configuration
====================

Any Zotonic system is capable of sending and receiving e-mail messages
over SMTP.

Zotonic implements a mailing system for sending text and HTML messages
to one or more recipients.

Out of the box, e-mail sending should "just work".

Feedback
--------

If you need feedback on messages that have been sent, enable
:ref:`mod_logging` which provides an overview of sent/received and
bounced messages.

Site-specific settings
----------------------

+----------+--------------+-----------------------------------------+
|Module    |Key           |Value                                    |
+==========+==============+=========================================+
|site      |email_from    |Set this to the from-address you want to |
|          |              |e-mail to appear from, e.g. something    |
|          |              |like noreply@yoursite.com.               |
+----------+--------------+-----------------------------------------+
|site      |email_override|If set, all e-mail messages that get sent|
|          |              |from zotonic will arrive at this         |
|          |              |address. Usefull if you are testing but  |
|          |              |don't want to confuse other people with  |
|          |              |your test e-mails.                       |
+----------+--------------+-----------------------------------------+
|site      |smtphost      |The hostname where you want messages to  |
|          |              |appear from. Mostly used for bounce      |
|          |              |message handling and the EHLO            |
|          |              |handshake. Defaults to the site's        |
|          |              |hostname, but can be overriden           |
+----------+--------------+-----------------------------------------+
|zotonic   |admin_email   |E-mail address of the admin user, the    |
|          |              |address where admin log/debug messages   |
|          |              |get sent to when using                   |
|          |              |``z_email:send_admin/3``.                |
+----------+--------------+-----------------------------------------+

Zotonic-wide settings
---------------------

The file ``priv/config`` can be configured to hold any of the
configuration options below. They are in effect for every site running
in the Zotonic instance.

+------------------+--------------------------------------+
|Key               |Description                           |
+==================+======================================+
|smtp_relay        |Whether or not to use a SMTP relay    |
|                  |host. Boolean value, defaults to      |
|                  |false.                                |
+------------------+--------------------------------------+
|smtp_host         |The hostname for the SMTP relay host, |
|                  |only needed if smtp_relay is enabled. |
+------------------+--------------------------------------+
|smtp_ssl          |Whether or not to use SSL on the relay|
|                  |host, only needed if smtp_relay is    |
|                  |enabled.                              |
+------------------+--------------------------------------+
|smtp_username     |The username for the relay host, only |
|                  |needed if smtp_relay is enabled.      |
+------------------+--------------------------------------+
|smtp_password     |The password for the relay host, only |
|                  |needed if smtp_relay is enabled.      |
+------------------+--------------------------------------+
|smtp_no_mx_lookups|Set to true to not do a MX lookup     |
|                  |before sending mail. (default: false) |
+------------------+--------------------------------------+
|smtp_verp_as_from |Use the "from" address as VERP for    |
|                  |bounce handling (default: false)      |
+------------------+--------------------------------------+
|smtp_bcc          |Optionally send a BCC of every sent to|
|                  |this address                          |
+------------------+--------------------------------------+
|email_override    |A global e-mail override. The override|
|                  |logic first checks the site override, |
|                  |and then the global override address. |
+------------------+--------------------------------------+
|smtp_spamd_ip     |Optional IP address for a spamassassin|
|                  |host                                  |
+------------------+--------------------------------------+
|smtp_spamd_port   |Optional port number for a            |
|                  |spamassassin host                     |
+------------------+--------------------------------------+
|smtp_bounce_domain|Which domain to use for bounce VERP   |
|                  |messages. Defaults to the smtp domain.|
+------------------+--------------------------------------+


Sending E-mail
--------------

Once configured, you can use the following Erlang commands to send
e-mail from Zotonic code:

+-------------------------+--------------------------------------------------+
|Command                  |Explanation                                       |
+=========================+==================================================+
|``z_email:send_admin/3`` |Sends a quick e-mail to the site                  |
|                         |administrator. Handy to notice the site admin that|
|                         |something is wrong, a job has finished, etc... The|
|                         |e-mail that is used is the admin_email address    |
|                         |that is specified in the site's config file.      |
+-------------------------+--------------------------------------------------+
|``z_email:send/4``       |Sends a text message with a subject to a specified|
|                         |recipient.                                        |
+-------------------------+--------------------------------------------------+
|``z_email:send_render/4``|Renders a template and sends it as a HTML message |
|                         |to a specified recipient.                         |
+-------------------------+--------------------------------------------------+
