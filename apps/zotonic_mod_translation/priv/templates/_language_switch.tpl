
{% with icon_after|default:'<i class="caret"><span class="visually-hidden">{_ Open language menu _}</span></i>' as icon_after %}
{% if m.translation.language_list_enabled as list %}
{% if list|length > 1 %}
    {% if is_nav %}
        {# Bootstrap nav item #}
        <li class="nav-item dropdown" id="languages">
            <a href="#languages" class="nav-link dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" data-toggle="dropdown">
                {{ z_language|upper }} {{ icon_after }}
            </a>

            <ul class="dropdown-menu admin-dropdown-menu-has-icons">
                {% if id %}
                    {% for code,lang in list %}
                        <li>
                            <a href="{{ id.page_url with z_language = code }}" class="dropdown-item">
                                {% if z_language == code %}<i class="fa-solid fa-check"></i>{% endif %}
                                {{ lang.name }}
                            </a>
                        </li>
                    {% endfor %}
                {% else %}
                    {% for code,lang in list %}
                        <li>
                            <a href="#" id="{{ #l.code }}" class="dropdown-item">
                                {% if z_language == code %}<i class="fa-solid fa-check"></i>{% endif %}
                                {{ lang.name }}
                            </a>
                        </li>
                        {% wire id=#l.code postback={set_language code=code id=id} delegate="mod_translation" %}
                    {% endfor %}
                {% endif %}
            </ul>
        </li>
    {% else %}
        {# Simple select list #}
    	<select class="form-control" id="{{ #lang }}">
        	{% for code,lang in list %}
       			<option {% if z_language == code %}selected="selected"{% endif %} value="{{ code }}">{{ lang.name }}</option>
        	{% endfor %}
    	</select>
    	{% wire id=#lang type="change" postback={set_language id=id} delegate="mod_translation" %}
    {% endif %}
{% endif %}
{% endif %}
{% endwith %}