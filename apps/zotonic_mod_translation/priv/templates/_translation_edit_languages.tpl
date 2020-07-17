{% block language_options %}
{% with m.rsc[id].language as r_lang %}
<div class="form-group">
    <div id="admin-translation-checkboxes">
        {% for code, lang in m.translation.language_list_configured %}
            {% if lang.is_editable %}
            <label class="checkbox-inline">
    	    <input type="checkbox" id="{{ #language.code }}" name="language[]" value="{{ code }}"
    	           {% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %} />
    	    <span {% include "_language_attrs.tpl" language=code %}>{{ lang.name_en }}</span>
            </label>
            {% endif %}
        {% empty %}
            <div class="checkbox"><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></div>
        {% endfor %}
    </div>
</div>
{% endwith %}

{% javascript %}
    $('#admin-translation-checkboxes input').on('change', function() {
        let code = $(this).attr('value');
        if ($(this).is(":checked")) {
            $(".tab-"+code).show();
            if (!$(".language-tabs .active a").is(":visible")) {
                $(".tab-"+code+" a").get(0).click();
            }
        } else {
            $(".tab-"+code).hide();
            if ($(".tab-"+code).hasClass("active")) {
                let vis = $(".language-tabs a:visible").get(0);
                if (vis) {
                    vis.click();
                } else {
                    $(".tab-"+code).removeClass("active");
                    $(".tab-pane.language-"+code).removeClass("active");
                }
            }
        }
    });
{% endjavascript %}

{% endblock %}
