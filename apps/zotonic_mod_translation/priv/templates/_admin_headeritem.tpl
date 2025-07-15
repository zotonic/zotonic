{% with icon_after|default:[
        '<i class="caret"><span class="sr-only">',
            _"Open language menu",
        '</span></i>'
    ] as icon_after %}
{% with m.translation.language_list_enabled as list_enabled %}
{% with m.translation.language_list_editable as list_editable %}
{% if list_editable|length > 1 %}
    <li class="dropdown" id="languages">
        <a class="dropdown-toggle" data-toggle="dropdown" href="#languages">
            {{ icon_before }} {{ z_language|upper }} {{ icon_after }}
        </a>
        <ul class="dropdown-menu mod_translation_menu-has-icons">
            {% for code,lang in list_enabled %}
                <li>
                    <a href="#" id="{{ #l.code }}">
                        {% if z_language == code %}<i class="glyphicon glyphicon-ok"></i>{% endif %}
                        {{ lang.name }}
                    </a>
                </li>
                {% wire id=#l.code postback={set_language code=code} delegate="mod_translation" %}
            {% endfor %}
            {% for code,lang in list_editable -- list_enabled %}
                {% if forloop.first %}
                    <li role="separator" class="divider"></li>
                    <li class="dropdown-header">{_ Editable languages _}</li>
                {% endif %}
                <li>
                    <a href="#" id="{{ #l.code }}">
                        {% if z_language == code %}<i class="glyphicon glyphicon-ok"></i>{% endif %}
                        {{ lang.name }}
                    </a>
                </li>
                {% wire id=#l.code postback={set_language code=code} delegate="mod_translation" %}
            {% endfor %}
        </ul>
    </li>
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
