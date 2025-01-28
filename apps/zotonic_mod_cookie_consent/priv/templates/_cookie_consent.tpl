{% with m.rsc.cookie_consent.id as id %}
<div id="cookie-consent" class="cookie-consent" role="dialog" tabindex="-1" aria-label="Privacy">
    <form id="cookie-consent__form" action="#">
        <div class="cookie-consent__upper">
            <h2 class="cookie-consent__title">{{ id.title|default:_"This website uses cookies" }}</h2>
            <p>{{ id.summary }}</p>
            {% if id.o.relation as rel_id %}
                <a href="{{ rel_id.page_url }}" target="_blank" class="cookie-consent-privacy">{{ rel_id.title }}</a>
            {% elseif id.body %}
                <a href="{{ id.page_url }}" target="_blank" class="cookie-consent-privacy">{_ More information about cookies _}</a>
            {% endif %}
            <div class="cookie-consent__upper__controls">
                <button class="cookie-consent-toggle">
                    {_ Change settings _}
                </button>
                <button class="btn btn-primary" type="submit" id="cookie-consent__accept-all">
                    {_ Accept all _}
                </button>
                <button class="btn btn-primary" type="submit" id="cookie-consent__accept-none">
                    {_ Reject all _}
                </button>
            </div>
        </div>

        <div class="cookie-consent__lower" style="display: none">
            <ul class="cookie-options">
                <li>
                    <input id="cookie_consent_functional" name="cookie_consent_functional" type="checkbox" disabled="" checked>
                    <label for="cookie_consent_functional">{_ Functional _}</label>
                </li>
                {% if id.cookies_statistics %}
                    <li>
                        <input id="cookie_consent_statistics" name="cookie_consent_statistics" type="checkbox">
                        <label for="cookie_consent_statistics">{_ Statistics _}</label>
                    </li>
                {% endif %}
                {% if id.cookies_marketing %}
                    <li>
                        <input id="cookie_consent_all" name="cookie_consent_all" type="checkbox">
                        <label for="cookie_consent_all">{_ All cookies _}</label>
                    </li>
                {% endif %}
            </ul>
            <ul class="cookie-options-explanations">
                <li>
                    <h3>{_ Functional _}</h3>
                    <p>{{ id.cookies_functional }}</p>
                </li>
                {% if id.cookies_statistics %}
                    <li>
                        <h3>{_ Statistics _}</h3>
                        <p>{{ id.cookies_statistics }}</p>
                    </li>
                {% endif %}
                {% if id.cookies_marketing %}
                    <li>
                        <h3>{_ All cookies _}</h3>
                        <p>{{ id.cookies_marketing }}</p>
                    </li>
                {% endif %}
            </ul>
            <div class="cookie-options-accept">
                <button class="btn btn-default" id="cookie-consent__accept-selection" type="submit">
                    {_ Accept selection _}
                </button>
            </div>
        </div>
    </form>

</div>
{% endwith %}

{% javascript %}
    document.getElementById('cookie-consent__form').focus();

    switch (z_cookie_consent_fetch()) {
        case 'all':
            $('#cookie_consent_all').prop('checked', true);
            $('#cookie_consent_statistics').prop('checked', true);
            break;
        case 'stats':
            $('#cookie_consent_statistics').prop('checked', true);
            break;
        default:
            break;
    }

    $('#cookie-consent__accept-all').on('click', function(ev) {
        ev.preventDefault();
        $('#cookie-consent').remove();
        z_cookie_consent_store('all')
    });

    $('#cookie-consent__accept-none').on('click', function(ev) {
        ev.preventDefault();
        $('#cookie-consent').remove();
        z_cookie_consent_store('functional');
    });

    $('.cookie-consent-toggle').on('click', function(ev) {
        ev.preventDefault();
        $(".cookie-consent__lower").slideToggle();
    });

    $('#cookie_consent_all').on('change', function() {
        if ($(this).is(":checked")) {
            $('#cookie_consent_statistics').prop('checked', true);
        }
    });

    $('#cookie_consent_statistics').on('change', function() {
        if (!$(this).is(":checked")) {
            $('#cookie_consent_all').prop('checked', false);
        }
    });

    $('#cookie-consent__form').on('submit', function(ev) {
        ev.preventDefault();
        if ($('#cookie_consent_all').is(":checked")) {
            z_cookie_consent_store('all')
        } else if ($('#cookie_consent_statistics').is(":checked")) {
            z_cookie_consent_store('stats')
        } else {
            z_cookie_consent_store('functional');
        }
        $('#cookie-consent').remove();
    });
{% endjavascript %}
