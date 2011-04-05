{% extends "admin_edit_widget_std.tpl" %}

{# A map admin_edit widget #}


{% block widget_title %}{_ Geodata _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
<fieldset class="admin-form">
	<div class="notification notice">
		{_ Please click on the map to select the location. _}
		<a href="javascript:;" id="fill-geo">{_ Try to fill the fields automatically from the title of the location _}</a>
	</div>

	<div class="zp-15">
		<div class="form-item clearfix">
			<label for="location_lat">{_ Latitude _}</label>
			<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" style="width: 60%" />
		</div>
	</div>

	<div class="zp-15">
		<div class="form-item clearfix">
			<label for="location_lng">{_ Longitude _}</label>
			<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" style="width: 60%" />
		</div>
	</div>
</fieldset>

<div id="map_canvas" class="map-wrapper"></div>
{% endblock %}


{% block widget_after %}
<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
{% lib
	"js/modules/z.maps.js"
	"js/admin_edit_location.js"
%}
{% endblock %}
