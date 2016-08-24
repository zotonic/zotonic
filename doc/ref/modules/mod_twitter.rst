.. highlight:: django
.. include:: meta-mod_twitter.rst

Import Twitter updates in your Zotonic site in realtime.

The Twitter module allows you to have Tweets ‘mirrored’ from Twitter
into your Zotonic site. It does this by creating a background process
which has a continuous HTTP connection to the Twitter Streaming
API. This way, Tweets arrive instantly at your Zotonic site, no
polling necessary!

Installation
------------

Enable the mod_twitter module. Then

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

At this point the module will login to Twitter and start following the
user(s) that you entered their Twitter IDs for. Only `public (or unprotected)`_
Tweets will be imported.

Domain model
------------

The :term:`domain model` for this module is the following: the module
creates a :ref:`category <guide-datamodel-categories>` `Tweet` as a
subcategory of `text.` Each time a Tweet is imported, a resource of
this category is created.

From the Tweet there is an `author` edge to the person that created
the Tweet (i.e. the user who you set the Twitter ID on).

The body text of the Tweet resource is HTML will already be
preprocessed with the :ref:`filter-twitter` filter, so URLs, hash-tags
and @-links are converted to HTML already when the Tweet is saved.

The original value of the Tweet (the JSON object received from Twitter)
is stored inside the ``tweet`` property in the Tweet resource, and can be
displayed like this::

    {% print m.rsc[id].tweet %}

Using logon with Twitter
------------------------

Add an app on Twitter and get the consumer key / secret. In Zotonic,
configure two config keys, ``mod_twitter.consumer_key`` and
``mod_twitter.consumer_secret`` to contain these values. Now set up
the callback URL to your Zotonic site in the Twitter app. The logon
window will now automatically show a "Sign in with Twitter" button.

.. seealso:: :ref:`filter-twitter` filter

.. _public (or unprotected): https://support.twitter.com/articles/20169886
