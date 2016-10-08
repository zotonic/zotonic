.. highlight:: django
.. include:: meta-mod_twitter.rst

Import Twitter updates in your Zotonic site in realtime.

The Twitter module allows you to have Tweets ‘mirrored’ from Twitter
into your Zotonic site. It does this by creating a background process
which has a continuous HTTP connection to the Twitter Streaming
API. This way, Tweets arrive instantly at your Zotonic site, no
polling necessary!

Twitter login
-------------

First, create an app on Twitter. Point it’s callback URL to your Zotonic site.
Then in your site, :ref:`activate <activating-modules>` mod_twitter. Head to
‘Auth’ > ‘External services’ in the admin interface to enter your Twitter app’s
consumer key and secret. Enable Twitter login by checking the ‘Use Twitter
authentication’ box. This will add a ‘Log in with Twitter’ button to the logon
form on your site.

Importing Tweets
----------------

The mod_twitter module can import Tweets into Zotonic using Twitter’s
`Streaming API`_.

:ref:`Activate <activating-modules>` mod_twitter. Then head
to ‘Auth’ > ‘External services’ in the admin interface to enter your Twitter
access token and secret.

The module will follow any user(s) in your site that you entered their Twitter
ID for. You can so by going to a person’s page in the admin. In the edit
page’s right sidebar, you will find a new item, ‘Twitter ID’. Here, enter the
`numeric Twitter ID`_ of the user. Only `public (or unprotected)`_
Tweets will be imported.

Domain model
^^^^^^^^^^^^

The :term:`domain model` for this module is the following: the module
creates a :ref:`category <guide-datamodel-categories>` ‘Tweet’ as a
subcategory of ‘text’. Each time a Tweet is imported, a resource of
this category is created.

From the Tweet there is an ‘author’ edge to the person that created
the Tweet (i.e. the user who you set the Twitter ID on).

The body text of the Tweet resource is HTML will already be
preprocessed with the :ref:`filter-twitter` filter, so URLs, hashtags
and @-links are converted to HTML already when the Tweet is saved.

The original value of the Tweet (the JSON object received from Twitter)
is stored inside the ``tweet`` property in the Tweet resource, and can be
displayed like this::

    {% print m.rsc[id].tweet %}

.. seealso::

    * :ref:`mod_facebook`
    * :ref:`mod_instagram`

.. _public (or unprotected): https://support.twitter.com/articles/20169886
.. _Streaming API: https://dev.twitter.com/streaming/overview
.. _numeric Twitter ID: http://www.mytwitterid.com
