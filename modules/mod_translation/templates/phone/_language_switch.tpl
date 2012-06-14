{% with m.config.i18n.language_list.list as list %}
{% if list %}
{% if is_nav %}
    {# Bootstrap nav item #}
    <li class="dropdown" id="languages">
        <a class="dropdown-toggle" data-toggle="dropdown" href="{% url language_switch p=m.req.raw_path %}" data-target="#">
            {{ list[z_language].language|default:z_language }}
            <b class="caret"></b>
        </a>
        <ul class="dropdown-menu">
        {% for code,lang in list %}
		{% if all or lang.is_enabled %}
            <li>
                <a id="{{ #lang }}" href="{% url language_select code=code p=m.req.raw_path %}">{% if code == z_language %}<i class="icon-ok"></i> {% endif %}{{ lang.language }}</a>
            </li>
        	{% wire id=#lang type="change" postback={set_language} delegate="mod_translation" %}
        {% endif %}
        {% endfor %}
        </ul>
    </li>
{% else %}
    {# Simple select list #}
	<select id="{{ #lang }}">
	{% for code,lang in list %}
		{% if all or lang.is_enabled %}
			<option {% if z_language == code %}selected="selected"{% endif %} value="{{ code }}">{{ lang.language }}</option>
		{% endif %}
	{% endfor %}
	</select>
	{% wire id=#lang type="change" postback={set_language} delegate="mod_translation" %}
{% endif %}
{% endif %}
{% endwith %}
