{% extends "_editor.tpl" %}

{#
This template maintains the newest tinymce version.

params:
- overrides_tpl:          (optional) template location that contains JavaScript overrides for tinymce init
- zmedia_tabs_enabled:    Tabs enabled in the link dialog for the zmedia action
- zmedia_tabs_disabled:   Tabs disabled in the link dialog for the zmedia action (defaults to ["new"])
- zlink_tabs_enabled:     Tabs enabled in the link dialog for the zlink action
- zlink_tabs_disabled:    Tabs disabled in the link dialog for the zlink action (defaults to ["new"])
#}

{% block _editor %}
    {% with zlink_tabs_disabled|default:["new"] as zlink_tabs_disabled %}
        {% inherit %}
    {% endwith %}
{% endblock %}
