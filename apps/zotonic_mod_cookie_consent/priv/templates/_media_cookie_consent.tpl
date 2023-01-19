{# Wrapper for HTML that needs cookie consent #}

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
        <a href="#cookie-consent" id="{{ #consent }}" class="cookie-consent-change">{_ Change cookie settings _}</a>
    </p>

    
    {% javascript %}
    alert("bliep");
        document.getElementById("{{ #consent }}").addEventListener("click", () => {
            document.getElementById("cookie-consent").focus();
        });
    {% endjavascript %}

    <script type="text/x-cookie-consented">
        {{ html|escape }}
    </script>
</div>
{% endwith %}
{% endwith %}