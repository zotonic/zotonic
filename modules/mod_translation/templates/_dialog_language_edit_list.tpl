{#
Params:
code
initial_lang_code
#}
<div id="dialog_language_edit_content">
    <div class="mod_translation-filter">
        <input id="mod_translation_filter" class="form-control" type="text" value="" placeholder="Find language by name or code" autofocus="1" />
    </div>
    <div id="mod_translation_list" class="list-group mod_translation-language-list">
        {% for lang_code, lang_data in m.translation.main_languages %}
            {% with lang_data.sublanguages as sublanguages %}
                {% with lang_code ++ sublanguages|element:1 as all %}
                    <a class="list-group-item" id="{{ #item.lang_code }}" data-name="{{ lang_data.name_en|lower }}" data-code="{{ lang_code }} {% for lang_code1 in all %}{{ lang_code1 }}{% endfor %}">
                        <i class="fa fa-chevron-right pull-right"></i>
                        <h3>{{ lang_data.name_en }}</h3>
                        <h4 class="mod_translation-codes text-muted">
                            {% for lang_code1 in all %}
                                <span class="mod_translation-code {% if  m.translation.language_list_configured[lang_code1|as_atom] %}mod_translation-added{% endif %}">{{ lang_code1 }}</span>
                            {% endfor %}
                        </h4>
                    </a>
                {% endwith %}
            {% endwith %}
            {% wire id=#item.lang_code
                action={replace
                    template="_dialog_language_edit_detail.tpl"
                    target="dialog_language_edit_content"
                    code=lang_code
                    initial_lang_code=initial_lang_code
                }
            %}
        {% endfor %}
    </div>
</div>

{% javascript %}
var filterList = function(filter) {
    var $list = $("#mod_translation_list");
    var $links = $("a", $list);
    if (filter) {
        $links.each(function(i, el) {
            var $el = $(el);
            var name = $el.attr("data-name");
            var code = $el.attr("data-code");
            var value = name + " " + code;
            if (!value.match(filter)) {
                $el.hide();
            } else {
                $el.show();
            }
        });
    } else {
        $list.find("a").show();
    }
}
$("#mod_translation_filter").on("input", function(e) {
    filterList($(this).val().toLowerCase());
});
$.dialogCenter();
$.dialogScrollTo(0);
{% endjavascript %}
