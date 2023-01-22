{# Show the cookie consent dialog if consent is needed #}

{% wire name="cookie-consent"
    action={script script="
            if ($('#cookie-consent').length == 0) {
                    z_event('cookie-consent-banner', {});
            }
    "}
%}
{% wire name="cookie-consent-banner"
    action={
            insert_top 
            target=" body" 
            template="_cookie_consent.tpl"}
    action={script 
            script="window.setTimeout(() => document.getElementById('cookie-consent').focus(), 300);"}
%}

{% lib "js/z.cookie_consent.js" %}
