
.. include:: meta-mod_seo_sitemap.rst

Creates a ``sitemap.xml`` file for your site, containing links to all
publicly accessible pages.

`mod_seo_sitemap` creates a ``sitemap.xml`` file for which is used by
the Google bot to index your site. By default, its generated
sitemap.xml lists all pages of the categories text, event, location,
collection and person in the system which are publicly viewable.

Creating a sitemap without using mod_seo_sitemap
------------------------------------------------

If you have a site with mostly static URLS, it might be easier to
create your own sitemap XML file. If you create a dispatch rule like
this::

  {sitemap_xml, ["sitemap.xml"], controller_template, [{template, "mysitemap.tpl"}, {content_type, "text/xml"}]}
  
You will be able to write your own sitemap.xml file (in a
mysitemap.tpl template) without using mod_seo_sitemap. This might be
handy if you serve mostly static files.
