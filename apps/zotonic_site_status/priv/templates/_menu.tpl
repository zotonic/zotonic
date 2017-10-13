{% if m.acl.user %}
	<li><a {% if zotonic_dispatch == `zotonic_status` %}class="current"{% endif %}
            href="{% url zotonic_status %}">{_ Sites _}</a></li>

	<li><a id="{{ #logoff }}" href="#logoff">{_ Log Off _}</a></li>
	{% wire id=#logoff postback={logoff} %}
{% endif %}
