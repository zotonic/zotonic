.. highlight:: django
.. include:: meta-mod_mailinglist.rst

This module implements a mailing list system. You can make as many
mailing lists as you like and send any page to any mailing list,
including confirm mail and unsubscribe page.

Mailing lists are pages of the category `mailinglist`, which is
installed by the module upon activation. In the admin there is support
for managing these mailing lists where you can import, export, add or
delete recipients.

For details on configuring e-mail sending and receiving in Zotonic,
see :ref:`guide-email`.


Including the subscribe custom tag on your pages
------------------------------------------------

The module includes the :ref:`signup tag
<scomp-mailinglist_subscribe>` tag (and template) that you can use on
your site.

When you want to add a subscribe template to your page then you will
need the following scomp include in your template::

  {% mailinglist_subscribe id=mailing_id %}

Where ``mailing_id`` should be set to the page id of your mailing
list. This scomp includes the template
``_scomp_mailinglist_subscribe.tpl``.  The subscription form itself
can be found in the template “_mailinglist_subscribe_form.tpl”.  You
should overrule the latter template when you want to add or remove
fields from the subscription form.

It is possible to simplify the subscribe form by using the
option ``is_email_only``. When this parameter is used the users only
have to supply their email address to be added to the mailing list.
Example::

  {% mailinglist_subscribe id=mailing_id is_email_only %}


Displaying the number of subscribers
------------------------------------

It is possible to show the number of subscribers to a mailinglist by
retrieving this number from the mailinglist model. Example::

   {{ m.mailinglist.count_recipients[mailinglist_id] }} of subscribers

Pages for the mailing list category
-----------------------------------
The mailinglist module predefines the following dispatch rules for the mailinglist::

  /mailinglist/1234
  /mailinglist/1234/my-mailinglist-slug

Where 1234 stands for the id of your mailinglist (which is obviously another id).

The mailinglist page is very simple.  It shows the title, summary and
body of the mailinglist, followed by a subscribe form and finally a
list of other mailing lists.


Template used for the e-mails
-----------------------------

All e-mails use the :ref:`template-mailing_page` template.  It is a
very simple template that just tells why the recipient received the
e-mail, refers to the sent content page on the Internet and finally
shows the title, the summary and the body of the sent page.

In the footer there is a link to the unsubscribe page.

All e-mail templates extend from the :ref:`template-email_base`
template. The following templates are used for e-mails:

+---------------------------------------------------------+-------------------------------------------+
|Template                                                 |Description                                |
+=========================================================+===========================================+
|:ref:`template-email_mailinglist_confirm`                |Sent after subscribing to a mailing list,  |
|                                                         |requests to click on an url to confirm the |
|                                                         |subscription.                              |
+---------------------------------------------------------+-------------------------------------------+
|:ref:`template-email_mailinglist_goodbye`                |Sent after unsubscribing from a mailing    |
|                                                         |list.                                      |
+---------------------------------------------------------+-------------------------------------------+
|:ref:`template-email_mailinglist_welcome`                |Sent after subscribing and confirming the  |
|                                                         |subscription.                              |
+---------------------------------------------------------+-------------------------------------------+

Sending mailings in the admin
-----------------------------

On the resource edit page of any page (remember: the mailinglist
module can send `any` resource as a mailing!) there is an link in the
right column called `Go to the mailing page`.

The mailinglist recipients page
-------------------------------

Each mailinglist has a special page in the admin that lets you view
the recipients that are part of the list. On that page you can perform
varions functions on the recipients of the list.

- `Add new recipient` - Opens a dialog to add a single recipient to
  the list.

- `Download recipient list` - Downloads a ``.txt`` file with all the
  e-mail addresses and name details of all recipients.

- `Upload recipient list` - Upload a new file with recipients. Each
  e-mail address goes on its own line. There is a checkbox which lets
  you clear the list before the import, effectively overwriting all
  recipients in the list.

- `Clear recipient list` - After a confirmation, this removes all
  recipients from the list.

- `Combine two lists` - this opens a dialog which lets you combine two
  lists. Using this new dialog, the recipients of two lists can be
  combined according to the three set operations union, subtract and
  intersect.


The mailing status page
-----------------------

From the mailing status page you can send the current resource to a
mailing list, to the test mailing list or to an email address.

Sending mailings
................

The status page lists every mailing list in the system. On each row
you see how many recipients the list has, and the status, e.g. if the
mailing has already been sent to this list or not.

.. todo:: finish description of the mailing status page

Mailings are only send when the to be send page is published and
publicly visible.  The page should also be within its publication
period.

You can schedule a mailing by publishing the page but setting its
publication start date to the date and time you want your mailing to
be send.  The mailing list module checks every hour if there are any
mailings ready for sending.

An exception is made for the test mailing list, mailings to that
mailing list are always sent.

