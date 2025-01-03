.. highlight:: django
.. index:: tag; cache
.. _tag-cache:

cache
=====

Cache frequently used rendered template output for later reuse.

Cache the output of the enclosed cached template code.  The cached output can be named.  Cache tags with the same name will use each others cached entries, when the cache entry is still valid.

Example::

   {% cache 3600 now %}Local time is {% now "Y-m-d H:i:s" %}{% endcache %}

This caches the output of the enclosed template code for an hour.  The name for the cache is “now”.

The cache duration and name are optional. The default cache duration is 0 (zero) seconds, which gives parallel rendering protection (see below) though will not store the result in the cache.

The cache tag protects against the situation where parallel requests need to render the same template code at the same time. Instead of all the processes rendering the template code in parallel, one process will render the tem[plate code and share the result with the other waiting render processes.

Example, prevent parallel rendering (slam dunk) by non logged on visitors::

   {% cache if_anonymous %} insert complicated page here {% endcache %}

Besides the duration and the cache name the ``{% cache %}`` tag also accepts the following arguments:

+------------+----------------------------------------------------------------------------+--------------------+
|Argument    |Description                                                                 |Example             |
+============+============================================================================+====================+
|vary        |This argument can be used multiple times.  Cache blocks with different vary |vary=myid           |
|            |arguments have different cache keys.  The arguments are assumed to be keys  |                    |
|            |in the cache.  When one of the vary keys is updated or invalidated then the |                    |
|            |cached template code will be invalidated.                                   |                    |
+------------+----------------------------------------------------------------------------+--------------------+
|cat         |Category the cached output depends on. This argument can be used multiple   |cat="news"          |
|            |times for specifying multiple categories. The categories are not added to   |                    |
|            |the cache key, only added as cache dependencies.                            |                    |
+------------+----------------------------------------------------------------------------+--------------------+
|if          |Only cache the template output if the argument evaluates to true.           |if=can_cache        |
+------------+----------------------------------------------------------------------------+--------------------+
|if_anonymous|Only cache the block if the current visitor is not logged on (i.e. an       |if_anonymous        |
|            |anonymous visitor)                                                          |                    |
+------------+----------------------------------------------------------------------------+--------------------+
|visible_for |Sets the access control user for rendering the template.  With this you can |visible_for="world" |
|            |force to only show public items for logged on users.  Valid values are      |                    |
|            |“user”, 3, “group”, 2, “community”, 1, “world”, “public”, 0                 |                    |
+------------+----------------------------------------------------------------------------+--------------------+

The cache tag can be disabled by setting the config key ``mod_development.nocache``. This can be done on
the /admin/development page.