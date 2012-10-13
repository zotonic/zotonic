.. _manual-email:

E-mail handling
===============

Configuration
-------------

Any Zotonic system is capable of sending and receiving e-mail messages
over SMTP.

Zotonic implements a mailing system for sending text and HTML messages
to one or more recipients.

Out of the box, e-mail sending should "just work".

Feedback
........

If you need feedback on messages that have been sent, enable
:ref:`mod_logging` which provides an overview of sent/received and
bounced messages.

Site-specific settings
......................

+----------+--------------+-----------------------------------------+
|Module    |Key           |Value                                    |
+==========+==============+=========================================+
|site      |email_from    |Set this to the from-address you want to |
|          |              |e-mail to appear from, e.g. something    |
|          |              |like noreply@yoursite.com.               |
+----------+--------------+-----------------------------------------+
|site      |email_override|If set, all e-mail messages that get sent|
|          |              |from Zotonic will arrive at this         |
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
|site      |admin_email   |E-mail address of the admin user, the    |
|          |              |address where admin log/debug messages   |
|          |              |get sent to when using                   |
|          |              |``z_email:send_admin/3``.                |
+----------+--------------+-----------------------------------------+

The ``z_email:send_admin/3`` command actually looks in three different
places for determining the admin e-mail address: the config key
``zotonic.admin_email``, then the ``site.admin_email`` key, and
finally the `email` property of the admin user (user with id 1).


Zotonic-wide settings
.....................

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


The sender’s domain
...................

Recipients of e-mail want a valid sender’s address on the
envelope. This section describes how to set your e-mail bounce/sender
domain and fixing domain errors when sending e-mail.

You have to think of e-mail as normal snail mail. There is a message
and an envelope.

The ``email_from`` is the address that is written on the `message`.
It could be anything, and is generally not used when delivering your
mail. Just like with snail mail, the postman only looks on the
`envelope`, not on the message.

The address on the envelope is the most important address. It is where
your e-mail is returned to when the message can‘t be delivered (so
called bounces). Often it is also checked for validity when the e-mail
is delivered at a SMTP server.

You need a valid domain for this envelope sender address. The part
before the ``@`` is generated by Zotonic and is used for identifying
the original message and recipient when the message bounces.

How does Zotonic know the domain?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It checks in order:

- global priv/config: ``smtp_bounce_domain`` setting
- site's config: ``smtphost``  (you can also set this with the admin config as site.smtphost)
- site's config: ``hostname``

The part before the ``@`` is generated by Zotonic itself, for
administration and detection of bounces. A typical sender address on
the envelope looks like: ``noreply+mlcm6godbz2cchtgdvom@example.org``


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


Receiving E-mail
----------------

In its default configuration, Zotonic starts an SMTP server on port
2525 for receiving e-mail messages. You can write your own code to
decide what happens when somebody sends e-mail to the system, by
implementing the ``email_received`` notification (see below).

The SMTP server is also used to receive bounce messages from other
servers, when sending of a message has failed. :ref:`mod_mailinglist`
uses this functionality to automatically deactivate invalid e-mail
addresses.

Configuring incoming E-mail
...........................

To send messages to Zotonic, the domain part of the e-mail address
should have an A or MX record which points to the server where Zotonic
is able to receive on port 25. This means that you have to add a
firewall rule to redirect port 25 to 2525.

If you were to set up e-mail receiving for a site called
``example.com``, you could test if this is working by using the `netcat`
program, like this::

  nc example.com 25

Then, you should be greeted by Zotonic in the following way::

  220 example.com ESMTP Zotonic 0.9.0

Press ctrl-c to exit.

Handling incoming E-mail
........................

When receiving an e-mail message, Zotonic looks at the domain part of
the e-mail address to determine which :term:`Zotonic site` is
configured to handle this message. It looks at the ``host`` and
``hostalias`` fields in the site's config file to match the recipient
domain.

When no site matches the e-mails domain, the message is dropped, and a
warning logged.

For handling incoming messages in your site, you need a hook in your
site module to do something with the received messages, implementing
the ``email_receive`` notification.

.. highlight:: erlang

The code in your module looks like this::

  observe_email_received(E, _C) ->
      lager:warning("Email from: ~p: ~p", [E#email_received.from,
                                           E#email_received.email#email.subject]),
      ok.

Export this function and then restart your site. Now, send an e-mail
message to any address ``@example.com``, and notice that it arrives in
Zotonic::
      
  (zotonic001@host.local)9> 20:57:54.174 [warning] Email from: <<"arjan@miraclethings.nl">>: <<"Hello!">>


Troubleshooting
---------------

Check in the admin the log and smtp log. When a message bounces back
to the Zotonic SMTP server, you will see errors there. A typical error
looks like this::

  SMTP: bounce: 504 5.5.2 <noreply+mlcm6godbz2cchtgdvom@oeps>: Sender address rejected: need fully-qualified address To: piet@example.com (1234) From: <noreply+mlcm6godbz2cchtgdvom@oeps>
