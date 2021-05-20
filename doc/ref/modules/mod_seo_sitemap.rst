
.. include:: meta-mod_seo_sitemap.rst

Creates a ``sitemap.xml`` file for your site, containing links to all
publicly accessible pages.

`mod_seo_sitemap` creates a ``sitemap.xml`` file for which is used by
the Google bot to index your site. By default, its generated
sitemap.xml lists all pages of the categories text, event, location,
collection and person in the system which are publicly viewable.

The sitemaps follow the specification at https://www.sitemaps.org/protocol.html

Maintaining the sitemap index
-----------------------------

The ``seo_sitemap`` model maintains an index of all pages (and their URLs) that are publicly readable.

There is a button in the System > Status menu to rebuild the sitemap index. This is needed if there
are (big) changes in the access control affecting the publicly viewable pages or if languages are
changed.

If a resource is edited then all URLs of that resource are automatically added to the sitemap index tables.


Structure of the sitemap
------------------------

The generated sitemap is an index file with links to urlset files.

The urlsets links are generated from a combination of two sources:

 1. All resources and URLs in the sitemap index tables maintained by model ``seo_sitemap``
 2. Urlsets for by the ``#seo_sitemap_index{}`` notification.

The ``#seo_sitemap_index{}`` is a map notification where all modules can return 0 or more
maps with information about their pages. The map should be like::

    #{
        source => <<"wherefrom">>,          % Unique code
        count => 1234,                      % Number of URLs
        size => 5000,                       % (optional) number of URLs per urlset
        lastmod => {{2021,5,12}, {0,0,0}}   % (optional) last modification date
    }


The sitemap urlset is generated using the ``#seo_sitemap_urlset{}`` notification::

    #seo_sitemap_urlset, {
        source => <<"wherefrom">>,          % Unique code from the ``#seo_sitemap_index{}``
        offset => 1,                        % Offset for search, 1 based.
        limit => 50000                      % Number of URLs to be returned
    }

This is a *first* notification and should return a list of maps. Each map contains a single URL::

    #{
        loc => <<"https://example.com/...", % The URL for the urlset
        lastmod => {{2021,5,12}, {0,0,0}}}, % (optional) last modification date
        priority => 0.5,                    % (optional) priority, between 0.0..1.0
        changefreq => <<"weekly">>          % (optional) change frequency, for recrawls
    }


Creating a sitemap without using mod_seo_sitemap
------------------------------------------------

If you have a site with mostly static URLS, it might be easier to
create your own sitemap XML file. If you create a dispatch rule like
this::

  {sitemap_xml, ["sitemap.xml"], controller_template, [{template, "mysitemap.tpl"}, {content_type, "text/xml"}]}

You will be able to write your own sitemap.xml file (in a
mysitemap.tpl template) without using mod_seo_sitemap. This might be
handy if you serve mostly static files.

