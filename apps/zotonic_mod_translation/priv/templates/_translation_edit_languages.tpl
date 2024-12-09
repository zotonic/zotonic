{% block language_options %}
{% with m.rsc[id].language as r_lang %}
<div class="form-group">
    <ul id="admin-translation-checkboxes">
        {% for code, lang in m.translation.language_list_editable %}
            {% with code|member:r_lang or (not r_lang and z_language == code) as checked %}
            <label class="checkbox">
        	    <input type="checkbox" id="{{ #language.code }}" name="language[]" value="{{ code }}"
    	           {% if checked %}checked="checked"{% endif %} />
    	        <span {% include "_language_attrs.tpl" language=code %}>{{ lang.name }}</span>
                <span class="text-muted">&ndash; {% if code != 'en' %}{{ lang.name_en }}{% endif %} ({{ code }})</span>
            </label>
            {% endwith%}
        {% empty %}
            <div class="checkbox"><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></div>
        {% endfor %}
    </ul>
</div>
{% endwith %}
{% endblock %}

{% block language_wires %}
{% wire name="translation-done"
        action={alert
            title=_"Ready"
            text = [
                _"All texts were translated.",
                "<br><br>",
                _"Please check the translations and save the page when you are ready."
            ]
        }
%}

{% wire name="translation-incomplete"
        action={alert
            title=_"Incomplete translation"
            text = [
                _"Not all texts could be translated.",
                _"The source language text was used for the missing translations.",
                "<br><br>",
                _"Please check the translations and save the page when you are ready."
            ]
        }
%}

{% wire name="translation-error"
        action={alert
            text = _"There was an error translating the texts. Please try again later."
            is_danger
        }
%}
{% endblock %}

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

    function fill_texts(src, dst, mapping, overwrite) {
        let is_complete = true;

        $('.tab-pane.edit-language-' + dst + ', .widget-content-lang-' + dst).each(function() {
            let $form;

            if ($(this).hasClass('widget-content-lang-' + dst)) {
                $form = $(this).parent();
            } else {
                $form = $(this).closest(".widget-content, .translations, form");
            }
            $("input,textarea", this).each(function() {
                if ((overwrite || $.trim($(this).val()) == '') && $(this).attr('name').includes("$")) {
                    const from_name = $(this).attr('name').split('$')[0] + '$' + src;
                    if (!from_name.endsWith("_json")) {
                        const from_val = $form.find('[name="' + from_name + '"]').val().trim();

                        if (from_val !== '') {
                            let to_val = from_val;
                            if (mapping[from_val]) {
                                to_val = mapping[from_val];
                            } else {
                                is_complete = false;
                            }
                            if ($(this).hasClass('z_editor-installed')) {
                                z_editor_remove($(this));
                                $(this).val(to_val);
                                z_editor_add($(this));
                            } else {
                                $(this).val(to_val);
                            }
                        }
                    }
                }
            });
        });
        return is_complete;
    }

    function collect_texts(src, dst, overwrite) {
        const texts = [];
        $('.tab-pane.edit-language-' + dst + ', .widget-content-lang-' + dst).each(function() {
            let $form;

            if ($(this).hasClass('widget-content-lang-' + dst)) {
                $form = $(this).parent();
            } else {
                $form = $(this).closest(".widget-content, .translations, form");
            }
            $("input,textarea", this).each(function() {
                if ((overwrite || $.trim($(this).val()) == '') && $(this).attr('name').includes("$")) {
                    const from_name = $(this).attr('name').split('$')[0] + '$' + src;
                    if (!from_name.endsWith("_json")) {
                        const from_val = $form.find('[name="' + from_name + '"]').val().trim();
                        if (from_val !== '') {
                            texts.push(from_val);
                        }
                    }
                }
            });
        });
        return texts;
    }

    cotonic.broker.subscribe("model/translation/post/submit", function(msg) {
        const value = msg.payload.value;
        const src = value.src.replace(/[^a-z0-9A-Z-]/g, '');
        const dst = value.dst.replace(/[^a-z0-9A-Z-]/g, '');
        const overwrite = value.overwrite ?? false;

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

        switch (value.method) {
            case 'copy':
                fill_texts(src, dst, [], overwrite);
                break;
            case 'translate':
                z_mask('body');
                const texts = collect_texts(src, dst, overwrite);
                cotonic.broker.call("bridge/origin/model/translation/get/translate", {
                        from: src,
                        to: dst,
                        texts: texts
                    }).then((msg) => {
                        if (msg.payload.status == 'ok') {
                            const result = msg.payload.result;
                            const mapping = {};
                            for (let i=0; i < result.length; i++) {
                                mapping[result[i].text] = result[i].translation;
                            }
                            if (fill_texts(src, dst, mapping, overwrite)) {
                                $("#trans-review-" + dst)
                                    .fadeIn()
                                    .find("input")
                                    .val("1");
                                z_event("translation-done");
                            } else {
                                z_event("translation-incomplete");
                            }
                        } else {
                            z_event("translation-error");
                        }
                        z_unmask('body');
                    });
                break;
            default:
                break;
        }
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
