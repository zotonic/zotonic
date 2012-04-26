{% extends "admin_edit_widget_std.tpl" %}

{# A map admin_edit widget #}

{% block widget_title %}{_ Geodata _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}content-location{% endblock %}

{% block widget_content %}
<div id="{{ #lazy }}">
    {% lazy action={update target=#lazy id=id template="_geomap_admin_location_map.tpl"}%}
</div>
{% endblock %}

{#
{% block widget_after %}
<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
{% lib
	"js/modules/z.maps.js"
	"js/admin_edit_location.js"
%}
{% endblock %}
#}
