<li class="dropdown" id="languages">
    {% with m.config.i18n.language_list.list as list %}
    {% if list %}
    <a class="dropdown-toggle" data-toggle="dropdown" href="#languages">
        {{ z_language|upper }}
        <b class="caret"></b>
    </a>
    <ul class="dropdown-menu">
	{% for code,lang in list %}
	{% if all or lang.is_enabled %}
	<li><a href="#" id="{{ #l.code }}">
            {% if z_language == code %}<i class="icon-ok"></i>{% else %}<i class="icon-ok icon-white"></i>{% endif %}
        {{ lang.language }}</a></li>
	{% wire id=#l.code postback={set_language code=code} delegate="mod_translation" %}
        {% endif %}
	{% endfor %}
    </ul>
    {% endif %}
    {% endwith %}
</li>
