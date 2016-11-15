
.. include:: meta-mod_seo.rst
.. highlight:: django

Adds basic search engine optimization to the base templates and
provides an admin interface for the SEO modules.

Google Analytics
----------------

To enable `Google Universal Analytics`_ tracking on your Zotonic website, go to
``http://yoursite/admin/seo`` in your browser and enter your Google Analytics
tracking code.

Zotonic automatically supports the `User ID Analytics feature`_ by adding the
user ID to the tracking code. You have to `enable User ID`_ in your Analytics
account first.

Extra parameters
^^^^^^^^^^^^^^^^

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
