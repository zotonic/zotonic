.. include:: meta-mod_cookie_consent.rst

Wrap external content in such a way that it is only loaded if the user consented to the
inclusion of the content (and subsequent cookies).

The consent is one of:

 - ``functional`` this is always allowed
 - ``stats`` if consent for statistics tracking is given
 - ``all`` for any other kind of cookies

For elements this defaults to ``all``. This means that they are only rendered if all consent is given.

How to use
----------

Ensure that your base template has an all-include of ``_html_head.tpl`` and ``_html_body.tpl``.

Also, if you are using IFRAMEs, JS or CSS that sets non-functional cookies, check the changes below.

HTML
----

Media embedded via mod_oembed or  mod_video_embed are automatically wrapped according
to this method::

    <figure class="cookie-consent-preview do_cookie_consent mediaclass-..." data-cookie-consent="all">
        <img src="..." alt="Media preview">
        <figcaption>Please consent to cookies to display external content.</figcaption>
        <script type="text/x-cookie-consented">
            {% filter escape %}
            <iframe width="560" height="315" src="https://www.youtube.com/embed/IdIb5RPabjw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
            {% endfilter %}
        </script>
    </figure>

If there is no script tag then the page is reloaded after cookies are accepted.


IFRAME
------

Use the ``data-cookie-consent-src`` attribute to define the ``src`` if the cookie consent has been
given::

    <iframe width="560" height="315" data-cookie-consent-src="https://www.youtube.com/embed/...."></iframe>


JAVASCRIPT
----------

Use the special ``type="text/x-cookie-consent"`` and optionally the ``data-cookie-consent="..."`` attribute::

    <script type="text/x-cookie-consent" data-cookie-consent="stats" src="https://..."></script>


CSS
---

Use the special ``type="text/x-cookie-consent"`` and optionally the ``data-cookie-consent="..."`` attribute::

    <link type="text/x-cookie-consent" data-cookie-consent="stats" href="https://..."></script>
