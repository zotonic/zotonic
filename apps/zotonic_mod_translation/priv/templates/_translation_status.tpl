{% with id.translation_status[lang_code] as status %}
    <div id="trans-review-{{ lang_code }}" class="help-block" {% if not status %}style="display:none"{% endif %}>
        <span class="glyphicon glyphicon-info-sign"></span>
        <b>{_ This translation has been automatically generated._}</b>
        <button id="trans-review-btn-{{ lang_code }}" class="btn btn-xs btn-primary" type="button">
            {_ Approve translation _}
        </button>
        <br>
        {_ Please review all texts and correct any mistakes. Approve the translation if it is correct. This message will then disappear. _}

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
