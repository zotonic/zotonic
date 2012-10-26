Add Chat to Your Zotonic Site
=============================

Thanks to Michael Connor's ``zchat``, it's easy to add chat system on
your Zotonic site.

Why
---

It is often very useful for a site to have a live support line
built-in.  Another case for chat is on a social site where you'd like
your members to be able to communicate in real time.

Assumptions
-----------

Readers are assumed to have a working Zotonic system running, and
Mercurial (the ``hg`` command) installed.

How
---

Install the ``mod_chat`` module using ``zotonic installmodule``::

  :~$ zotonic installmodule mod_chat

This should download ``mod_chat`` and install it in the
`priv/modules/` directory. Restart and rebuild zotonic to see the
effect of these changes::

  $ cd /home/zotonic/zotonic
  $ zotonic stop
  $ make
  $ zotonic start

Now, log into your site admin page, select `System` and then
`Modules`. Scan down to find mod_chat. Activate it.

Now you can either include chat into an existing page on your site or
create a new page.

To include chat in an existing page, find the template that formats
the page in the templates directory and somewhere within the content
block enter::

  {% include "_chat_box.tpl" %}

To create a new page:

Create a page called "Chat" with category Text. Give it the Unique
name ``page_chat``, click on the Published check box, and save.

Add the following dispatch rule to your dispatch list::

  {chat, ["chat"], controller_page, [ {template, "chat.tpl"}, {id, page_chat} ]},

If this is the last rule in your list, omit the trailing comma.

Go to `System` and `status` in your admin menu and click on `rescan
modules`.

Now you should be able to go to ``<yoururl>/chat`` and see your chat
window. You'll see `Anonymous <long number>` in the right-most
pane. That's you. If your chat buddy enters the chat page, a second
Anonymous <another big number> will appear. When you or your buddy
leave the chat page, the respective Anonymous tag will disappear. Note
that chat messages are not stored or logged. That may come with a
future version of zchat.
