{% with m.rsc.cookie_consent.id as id %}
<div id="cookie-consent" class="cookie-consent">

    <form id="cookie-consent__form" action="#">
        <div class="cookie-consent__upper">
            <h2 class="cookie-consent__title">
                {{ id.title|default:_"This website uses cookies" }}
            </h2>
            <p class="cookie-consent__explanation">
                {{ id.summary }}
            </p>
            <div class="cookie-consent__upper__controls">
                <div class="cookie-consent__upper__text">
                    {% if id.o.relation[1] as rel_id %}
                        <a href="{{ rel_id.page_url }}" target="_blank" class="cookie-consent-privacy">{{ rel_id.title }}</a>
                    {% elseif id.body %}
                        <a href="{{ id.page_url }}" target="_blank" class="cookie-consent-privacy">{_ More information about cookies _}</a>
                    {% endif %}
                    <button class="cookie-consent-toggle">
                        {_ Change settings _}
                        <img class="anchor-down" alt="" src="/lib/images/arrow-right-thick.svg">
                    </button>
                </div>
                <div class="cookie-consent__upper__buttons">
                    <button class="cookie-consent__accept" type="submit" id="cookie-consent__accept-all">
                        {_ Accept alll _}
                    </button>
                </div>
            </div>
        </div>

        <div class="cookie-consent__lower" style="display: none">
            <ul class="cookie-consent__lower__options">
                <li class="cookie-option">
                    <input class="cookie-option__checkbox" id="cookie_consent_functional" name="cookie_consent_functional" type="checkbox" disabled="" checked>
                    <label for="cookie_consent_functional" class="cookie-option__label cookie-option__label cookie-option__label--disabled">
                        {_ Functional _}
                    </label>
                </li>
                <li class="cookie-option">
                    <input class="cookie-option__checkbox" id="cookie_consent_statistics" name="cookie_consent_statistics" type="checkbox">
                    <label for="cookie_consent_statistics" class="cookie-option__label">
                        {_ Statistics _}
                    </label>
                </li>
                {% if id.cookies_marketing %}
                    <li class="cookie-option">
                        <input class="cookie-option__checkbox" id="cookie_consent_all" name="cookie_consent_all" type="checkbox">
                        <label for="cookie_consent_all" class="cookie-option__label">
                            {_ All cookies _}
                        </label>
                    </li>
                {% endif %}
            </ul>
            <ul class="cookie-consent__lower__explanations">
                <li class="cookie-explanation">
                    <h4 class="cookie-explanation__title">{_ Functional _}</h4>
                    <p class="cookie-explanation__description">
                        {{ id.cookies_functional }}
                    </p>
                </li>
                <li class="cookie-explanation">
                    <h4 class="cookie-explanation__title">{_ Statistics _}</h4>
                    <p class="cookie-explanation__description">
                        {{ id.cookies_statistics }}
                    </p>
                </li>
                {% if id.cookies_marketing %}
                    <li class="cookie-explanation">
                        <h4 class="cookie-explanation__title">{_ All cookies _}</h4>
                        <p class="cookie-explanation__description">
                            {{ id.cookies_marketing }}
                        </p>
                    </li>
                {% endif %}
            </ul>
            <div class="cookie-consent__lower__accept">
                <button class="cookie-consent__accept" id="cookie-consent__accept-selection" type="submit">
                    {_ Accept selection _}
                </button>
            </div>
        </div>
    </form>

</div>
{% endwith %}

{% javascript %}
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
            z_cookie_consent_store('functional')
        }
        $('#cookie-consent').remove();
    });
{% endjavascript %}
