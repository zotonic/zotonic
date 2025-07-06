{% with icon_after|default:[
        '<i class="caret"><span class="sr-only">',
            _"Open language menu",
        '</span></i>'
    ] as icon_after %}
{% if m.translation.language_list_enabled as list %}
{% if list|length > 1 %}
    <li class="dropdown" id="languages">
        <a class="dropdown-toggle" data-toggle="dropdown" href="#languages">
            {{ icon_before }} {{ z_language|upper }} {{ icon_after }}
        </a>
        <ul class="dropdown-menu mod_translation_menu-has-icons">
            {% if id %}
                {% for code,lang in list %}
                    <li>
                        <a href="{{ id.page_url with z_language = code }}">
                            {% if z_language == code %}<i class="glyphicon glyphicon-ok"></i>{% endif %}
                            {{ lang.name }}
                        </a>
                    </li>
                {% endfor %}
            {% else %}
                {% for code,lang in list %}
                    <li>
                        <a href="#" id="{{ #l.code }}">
                            {% if z_language == code %}<i class="glyphicon glyphicon-ok"></i>{% endif %}
                            {{ lang.name }}
                        </a>
                    </li>
                    {% wire id=#l.code postback={set_language code=code} delegate="mod_translation" %}
                {% endfor %}
            {% endif %}
        </ul>
    </li>
{% endif %}
{% endif %}
{% endwith %}
