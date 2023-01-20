{# Wrapper for HTML that needs cookie consent                                  #}
{# This wrapper provides HTML - javascript tags and wires will not be executed #}
{# Clicks on #cookie-consent are handled by z.cookie_consent.js                #}
{% with consent|default:"all" as consent %}
{% with viewer_options.mediaclass|default:"cookie-consent-preview" as mediaclass %}
<div class="cookie-consent-preview do_cookie_consent mediaclass-{{ mediaclass }}" data-cookie-consent="{{ consent }}">
    {% image id.depiction
             mediaclass=mediaclass
             alt=_"Media preview"
             title=_"Click to change cookie settings"
    %}
    <p>
        {_ Please consent to cookies to display external content. _}
        <a href="#cookie-consent" class="cookie-consent-change">{_ Change cookie settings _}</a>
    </p>
    <script type="text/x-cookie-consented">
        {{ html|escape }}
    </script>
</div>
{% endwith %}
{% endwith %}