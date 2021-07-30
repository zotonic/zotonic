{% block language_options %}
{% with m.rsc[id].language as r_lang %}
<div class="form-group">
    <div id="admin-translation-checkboxes">
        {% for code, lang in languages %}
            {% if lang.is_enabled or lang.is_editable %}
            <label class="checkbox-inline">
    	    <input type="checkbox" id="{{ #language.code }}" name="language" value="{{ code }}"
    	           {% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %} />
    	    <span {% include "_language_attrs.tpl" language=code %}>{{ lang.language }}</span>
            </label>
            {% endif %}
        {% empty %}
            <div class="checkbox"><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></div>
        {% endfor %}
    </div>
</div>

{% javascript %}
    $('#admin-translation-checkboxes input').on('change', function() {
        var enabled = {};
        var tabBars = $(".language-tabs");
        var firstLang = false;

        $('#admin-translation-checkboxes input').each( function() {
            var isActive = $(this).is(':checked');
            var lang = $(this).attr('value');

            enabled[lang] = isActive;

            if (isActive && !firstLang) {
                firstLang = lang;
            }
        });

        if (tabBars.length > 0) {
            var activeIsDisabled = false;
            var activeIsEnabled = false;
            var tabs = tabBars.get(0).getElementsByTagName('li');

            for (var i=0; i < tabs.length; i++) {
                var lang = tabs[i].getAttribute('lang');

                if (enabled[lang]) {
                    if (tabs[i].style.display == 'none') {
                        $(".language-tabs li[lang="+lang+"]").show();
                    } else if ($(tabs[i]).hasClass('active')) {
                        activeIsEnabled = true;
                    }
                } else {
                    if (tabs[i].style.display != 'none') {
                        $(".language-tabs li[lang="+lang+"]").hide();
                    }
                    if ($(tabs[i]).hasClass('active')) {
                        activeIsDisabled = true;
                        $('.tab-pane.active.language-'+lang).removeClass('active');
                    }
                }
            }

            if ((activeIsDisabled || !activeIsEnabled) && firstLang !== false) {
                $(tabBars).find('li[lang='+firstLang+'] a').click();
                $('.tab-pane.language-'+firstLang).addClass('active');
            }
        }

    });
{% endjavascript %}

{% endwith %}
{% endblock %}
