{# Called from _admin_translation_tabs.tpl - Appends to the language tabs on top of the edit window #}
{% if top or button %}
    {% if top %}
        <li class="tab-action">
            <a href="#" role="button" id="language-add" title="{_ Add a new language or change languages. _}">
                + {_ Translate _}
            </a>
        </li>
    {% else %}
        <a class="btn btn-primary" href="#" role="button" id="language-add" title="{_ Add a new language or change languages. _}">
            {_ Add translation _}
        </a>
    {% endif %}

    {% javascript %}
        $('#language-add').on('click', (e) => {
            e.preventDefault();
            z_editor_save($('body'));

            const active = $('#rscform .language-tabs li.active').attr('lang');
            const enabled = [];
            const langs = $('input[name="language[]"]:checked');
            for (let i=0; i<langs.length; i++) {
                enabled.push(langs.get(i).value);
            }
            z_event("language-add", { language: active, enabled: enabled });
        });
    {% endjavascript %}

    {% wire name="language-add"
            action={dialog_open
                title=_"Translate"
                template="_dialog_rsc_language.tpl"
                id=id
            }
    %}

    {% if top %}
        <li class="tab-action">
            <a id="{{ #showtexts }}" href="{% url admin_translation_texts id=id close=1 %}" target="_blank"
               title="{_ Show all translated texts in a new tab. _}">
                {_ Show translations _}
            </a>
        </li>
    {% endif %}
{% endif %}
