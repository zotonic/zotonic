{# Show the cookie consent dialog if consent is needed #}

{% wire name="cookie-consent"
        action={insert_bottom target=" body" template="_cookie_consent.tpl"}
%}

{% lib "js/z.cookie_consent.js" %}
