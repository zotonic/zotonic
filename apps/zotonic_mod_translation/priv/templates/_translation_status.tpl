{% with id.translation_status[lang_code] as status %}
    <div id="trans-review-{{ lang_code }}" class="help-block" {% if not status %}style="display:none"{% endif %}>
        <p>
            <span class="glyphicon glyphicon-info-sign"></span>
            <b>{_ This translation has been automatically generated._}</b>
        </p>
        <p>
            <button id="trans-review-btn-{{ lang_code }}" class="btn btn-xs btn-primary" type="button">
                {_ Approve translation _}
            </button>
            <a class="btn btn-xs btn-default" href="{% url admin_translation_texts id=id close=1 %}" target="_showtexts" title="{_ Show all translated texts in a new tab. _}">
                {_ Show translations _} <span class="fa fa-external-link"></span>
            </a>
        </p>
        <p>
            {_ Please review all texts and correct any mistakes. Approve the translation if it is correct. This message will then disappear. _}
        </p>
        <input type="hidden" id="trans-status-{{ lang_code }}" name="translation_status.{{ lang_code }}" value="{{ status }}">
        <hr>
    </div>
{% endwith %}

{% wire id="trans-review-btn-" ++ lang_code
        action={confirm
            text=_"Did you review all texts and correct any mistakes?"
            ok=_"Yes"
            action={set_value target="trans-status-"++lang_code value=""}
            action={fade_out target="trans-review-" ++ lang_code}
        }
%}
