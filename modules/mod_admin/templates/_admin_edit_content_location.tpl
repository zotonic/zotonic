{% extends "admin_edit_widget_std.tpl" %}

{# A map admin_edit widget #}


{% block widget_title %}{_ Geodata _}{% endblock %}


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
<script type="text/javascript">
$(document).ready(function() {
	if($('#map_canvas').length) {	    
		googleMapsControl.buildMap({lat: 70.0, lng: -50.0, mapId: 'map_canvas', zoom: 2});

		window.marker = false; 
		var map = googleMapsControl.getMap();
				
		{% if m.rsc[id].location_lat and m.rsc[id].location_lng %}
			addMarker(new google.maps.LatLng({{ m.rsc[id].location_lat }}, {{ m.rsc[id].location_lng }}));
			setTimeout(function(){map.setCenter(window.marker.getPosition());},100);
		{% endif %}
				
		$('#fill-geo').click(function() {
			googleMapsControl.getGeoForAddress($('#field-title').val() || $('#field-title\\${{ z_language }}').val(), function(data) {
				if(window.marker) {
					removeMarker(window.marker);
				}
				
				addMarker(data.geometry.location);
				fillLatLng(data.geometry.location);
						
				google.maps.event.addListener(window.marker, 'dragend', function(e) {
					fillLatLng(window.marker.getPosition())
					map.setCenter(window.marker.getPosition());
				});

				google.maps.event.addListener(window.marker, 'drag', function(e) {
					fillLatLng(window.marker.getPosition());
				});
			})
		});
				
		google.maps.event.addListener(map, 'click', function(e)	{
			if(window.marker) {
				removeMarker(window.marker)
			}
			
			addMarker(e.latLng);
			
			google.maps.event.addListener(window.marker, 'click', function(e) {
				fillLatLng(window.marker.getPosition())
				map.setCenter(window.marker.getPosition());
			});

			clickMarker(window.marker);

			google.maps.event.addListener(window.marker, 'dragend', function(e) {
				fillLatLng(window.marker.getPosition())
				map.setCenter(window.marker.getPosition());
			});
			
			google.maps.event.addListener(window.marker, 'drag', function(e) {
				fillLatLng(window.marker.getPosition());
			});
		});
	}   //{# if $('#map_canvas').length #}
			
	function fillLatLng(pos) {
		$('#location_lat').val(pos.lat());
		$('#location_lng').val(pos.lng());
	}
	
	function addMarker(position) {
		window.marker = new google.maps.Marker({
			position:	position,
			map: 		map,
			icon:		'/lib/images/map_icon.png',
			flat: 		false,
			draggable:  true
		});

		google.maps.event.trigger(window.marker, "click");
		map.setCenter(window.marker.getPosition());
	}
			
	function removeMarker(marker) {
		marker.setMap(null);
	}

	function clickMarker(marker) {
		google.maps.event.trigger(marker, "click");
	}
});
</script>
{% endblock %}
