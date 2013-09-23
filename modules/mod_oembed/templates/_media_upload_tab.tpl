{# Tab title for the media embed tab #}
{% if not tabs_enabled or "oembed"|member:tabs_enabled %}
<li><a data-toggle="tab" href="#{{ tab }}-oembed">{_ Embed URL _}</a></li>
{% endif %}
