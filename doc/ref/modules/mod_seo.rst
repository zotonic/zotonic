
.. include:: meta-mod_seo.rst
.. highlight:: django

Adds basic search engine optimization to the base templates and provides an
admin interface for configuring SEO options and Google Universal Analytics.

SEO data
--------

mod_seo adds metadata to your pages to improve your website’s display and
ranking in search engine (e.g. Google) results. This metadata includes
`structured data`_:

    Google uses structured data that it finds on the web to understand the
    content of the page, as well as to gather information about the web and
    the world in general.

Following Google’s recommendations, mod_seo adds data in the `Schema.org`_
vocabulary, using the JSON-LD format. This enables `search features`_ such as
rich results, carousels and the sitelinks searchbox.

.. tip::

    If you wish to alter the structured data, you can do so by overriding the
    :file:`schema_org/schema.*.tpl` templates. When you do so, make sure to
    validate your changes with Google’s `structured data testing tool`_.

Configuration
-------------

Disable search engine indexing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To prevent search engines from indexing a page, check the ‘Ask Google not to
index this page’ checkbox in the admin. Programmatically, you can set the
``seo_noindex`` flag on a resource to do the same.

To disable indexing for the site as a whole, go to Modules > SEO in the Admin
(``https://yoursite/admin/seo``) and tick the ‘Exclude this site from search
engines’ checkbox.

Google Analytics
^^^^^^^^^^^^^^^^

To enable `Google Universal Analytics`_ tracking on your Zotonic website, go to
``http://yoursite/admin/seo`` in your browser and enter your Google Analytics
tracking code.

Zotonic automatically supports the `User ID Analytics feature`_ by adding the
user ID to the tracking code. You have to `enable User ID`_ in your Analytics
account first.

Extra parameters
""""""""""""""""

If you wish to add extra `Google Analytics parameters`_, override the
``_ga_params.tpl`` file and add the parameters::

    {#
        Override this template to provide extra Google Analytics parameters.
        See https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference
    #}
    {
        {% if m.acl.user %}
            "userId": "{{ m.acl.user|escapejs }}",
        {% endif %}
        "sessionControl": "start"
    }



.. todo:: Add more documentation

.. _Google Universal Analytics: https://support.google.com/analytics/answer/2790010
.. _User ID Analytics feature: https://support.google.com/analytics/answer/3123662
.. _enable User ID: https://support.google.com/analytics/answer/3123666
.. _Google Analytics parameters: https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference
.. _structured data: https://developers.google.com/search/docs/guides/intro-structured-data
.. _Schema.org: https://schema.org
.. _search features: https://developers.google.com/search/docs/guides/search-features
.. _structured data testing tool: https://search.google.com/structured-data/testing-tool
