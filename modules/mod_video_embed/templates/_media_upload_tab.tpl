{# Tab title for the media embed tab #}
{% if not tabs_enabled or "embed"|member:tabs_enabled %}
	<li><a data-toggle="tab" href="#{{ tab }}-embed">{_ Embed code _}</a></li>
{% endif %}
