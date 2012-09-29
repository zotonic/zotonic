
.. include:: meta-redirect.rst

Redirect to another url.

This controller redirects a request to another URL. The URL can be a
fixed URL, the location of a fixed page id or the name of a dispatch
rule.

Example dispatch rule using the redirect controller::

  {redir, ["plop"], controller_redirect, [{url, "/newplop"}, {is_permanent, true}]} 

This redirects any requests of "/plop" permanently to "/newplop".

It has the following dispatch options:


+-----------------+------------------------------------+-----------------------+
|Option           |Description                         |Example                |
+-----------------+------------------------------------+-----------------------+
|url              |The url of the new location the     |{url, "/example"}      |
|                 |browser is sent to.                 |                       |
+-----------------+------------------------------------+-----------------------+
|dispatch         |Name of a dispatch rule to use for  |{dispatch, admin}      |
|                 |the location url.  All arguments    |                       |
|                 |(except dispatch and is_permanent)  |                       |
|                 |are used as parameters for the      |                       |
|                 |dispatch rule.                      |                       |
+-----------------+------------------------------------+-----------------------+
|id               |Id of the page to redirect to. The  |{id, 123}              |
|                 |controller will redirect to the     |                       |
|                 |page_url of this id.  The id can be |                       |
|                 |an integer or the name of the page  |                       |
|                 |(use an atom or a binary).          |                       |
+-----------------+------------------------------------+-----------------------+
|qargs            |A list with querystring arguments to|{qargs, [id, slug]}    |
|                 |use in the new dispatch             |                       |
|                 |rule. Specifies what query (or      |                       |
|                 |dispatch) arguments to use from this|                       |
|                 |dispatch rule into the dispatch rule|                       |
|                 |that is being redirected to.        |                       |
|                 |                                    |                       |
+-----------------+------------------------------------+-----------------------+
|is_permanent     |Use a permanent (301) or temporary  |{is_permanent, false}  |
|                 |redirect (307). Defaults to false.  |                       |
+-----------------+------------------------------------+-----------------------+

This controller does only handle request arguments that are
specifically noted in the "qargs" list (and then only when the
"dispatch" argument is set).

Example
-------

A dispatch rule that always redirects /foo/12312/slug to /bar/12312/slug::

  {bar, ["bar", id, slug], controller_page, [{template, "bar.tpl"}]},
  {bar_redirect, ["foo", id, slug], controller_redirect, [{dispatch, bar}, {qargs, [id,slug]}]}
