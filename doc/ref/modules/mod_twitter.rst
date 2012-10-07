.. highlight:: django
.. include:: meta-mod_twitter.rst

Import Twitter updates in your Zotonic site in realtime.

The Twitter module allows you to have tweets "mirrored" from Twitter
into your Zotonic site. It does this by creating a background process
which has a continuous HTTP connection to the Twitter Streaming
API. This way, tweets arrive instantly at your Zotonic site, no
polling necessary!

Installation
------------
In the "config" section in the admin, create 2 new config keys:

``mod_twitter`` / ``api_login`` - Your Twitter username

``mod_twitter`` / ``api_password`` - Your Twitter password

We need your account details to startup the streaming API. mod_twitter
will never write to your account: it only reads your updates.

Now, activate the Twitter module under "Modules".

When you have activated the Twitter module, you should go to a
person's page in the admin. In the edit page, you see in the sidebar a
new item, called "Twitter ID". Here you can enter the numeric Twitter
ID of this user. Visit `My Twitter Id <http://www.mytwitterid.com/>`_
to find out your twitter ID.

At this point the module will login to Twitter and starts following the
user(s) that you entered their Twitter IDs for.

Domain model
------------

The :term:`domain model` for this module is the following: the module
creates a :ref:`category <manual-datamodel-categories>` `tweet` as a
subcategory of `text.` Each time a tweet is imported, a resource of
this category is created.

From the tweet there is an `author` edge to the person that created
the tweet (e.g. the user who you set the Twitter ID on).

The body text of the tweet resource is HTML will already be
preprocessed with the :ref:`filter-twitter` filter, so URLs, hash-tags
and @-links are converted to HTML already when the tweet is saved.

The original value of the tweet, e.g. the entire JSON object as
received from Twitter, is stored inside the ``tweet`` property in the
tweet resource, and can be displayed like this::

  {% print m.rsc[id].tweet %}


.. seealso:: :ref:`filter-twitter` filter
  
