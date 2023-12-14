{% block language_options %}
{% with m.rsc[id].language as r_lang %}
<div class="form-group">
    <div id="admin-translation-checkboxes">
        {% for code, lang in m.translation.language_list_editable %}
            <label class="checkbox-inline">
    	    <input type="checkbox" id="{{ #language.code }}" name="language[]" value="{{ code }}"
    	           {% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %} />
    	    <span {% include "_language_attrs.tpl" language=code %}>{{ lang.name }}</span>
            </label>
        {% empty %}
            <div class="checkbox"><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></div>
        {% endfor %}
    </div>
</div>
{% endwith %}

{% javascript %}
    cotonic.broker.subscribe("model/translation/post/disable", function(msg) {
        const lang = msg.payload.language;

        // Disable the language
        $('#admin-translation-checkboxes input').each( function() {
            if ($(this).attr('value') == lang) {
                $(this).prop('checked', false).trigger('change');
            }
        });
    });

    cotonic.broker.subscribe("model/translation/post/submit", function(msg) {
        const value = msg.payload.value;
        const src = value.src.replace(/[^a-z0-9A-Z-]/g, '');
        const dst = value.dst.replace(/[^a-z0-9A-Z-]/g, '');

        if (value.action) {
            if (value.action == 'dialog_close') {
                z_dialog_close();
            }
        }

        // Enable the language
        $('#admin-translation-checkboxes input').each( function() {
            if ($(this).attr('value') == dst) {
                $(this).prop('checked', true).trigger('change');

                $('.language-tabs li[lang='+dst+'] a').click();
                $('.tab-pane.language-'+dst).addClass('active');
            }
        });

        // Fill in the new language following the method selected

        switch (value.method) {
            case 'copy':
                // Copy src language to the dst language
                $('.tab-pane.edit-language-' + dst).each(function() {
                    let $form = $(this).closest("form");
                    $("input,textarea", this).each(function() {
                        if ($.trim($(this).val()) == '') {
                            let from_name = $(this).attr('name').split('$')[0] + '$' + src;
                            let from_val = $form.find('[name="' + from_name + '"]').val();
                            if ($(this).hasClass('z_editor-installed')) {
                                z_editor_remove($(this));
                                $(this).val(from_val);
                                z_editor_add($(this));
                            } else {
                                $(this).val(from_val);
                            }
                        }
                    });
                });
                break;
            case 'translate':
                // 1. Collect all translatable strings from src
                // 2. Make index
                // 3. Call translation api with (src, dst) language pair
                // 4. Use index to update the input fields
                break;
            default:
                break;
        }

        // src
        // dst
        // method
        console.log(value);
    });

    $('#admin-translation-checkboxes input').on('change', function() {
        let enabled = {};
        let tabBars = $(".language-tabs");
        let firstLang = false;

        $('#admin-translation-checkboxes input').each( function() {
            let isActive = $(this).is(':checked');
            let lang = $(this).attr('value');

            enabled[lang] = isActive;

            if (isActive && !firstLang) {
                firstLang = lang;
            }
        });


        if (tabBars.length > 0) {
            let activeIsDisabled = false;
            let activeIsEnabled = false;
            let tabs = tabBars.get(0).getElementsByTagName('li');

            for (let i=0; i < tabs.length; i++) {
                let lang = tabs[i].getAttribute('lang');

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

{% endblock %}
