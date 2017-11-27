{% if m.translation.language_list_enabled as list %}
    {% if is_nav %}
        {# Bootstrap nav item #}
        <li class="dropdown" id="languages">
            <a class="dropdown-toggle" data-toggle="dropdown" href="#languages">
                {{ z_language|upper }} <b class="caret"></b>
            </a>
            <ul class="dropdown-menu mod_translation_menu-has-icons">
                {% for code,lang in list %}
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
    {% else %}
        {# Simple select list #}
    	<select class="form-control" id="{{ #lang }}">
        	{% for code,lang in list %}
       			<option {% if z_language == code %}selected="selected"{% endif %} value="{{ code }}">{{ lang.language }}</option>
        	{% endfor %}
    	</select>
    	{% wire id=#lang type="change" postback={set_language} delegate="mod_translation" %}
    {% endif %}
{% endif %}
