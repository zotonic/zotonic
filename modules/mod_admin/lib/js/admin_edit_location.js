$(document).ready(function() {
	if($('#map_canvas').length) {
		googleMapsControl.buildMap({lat: 70.0, lng: -50.0, mapId: 'map_canvas', zoom: 2});

		window.marker = false;
		var map = googleMapsControl.getMap();

        if ($("#location_lat").val() && $("#location_lon").val())
        {
			addMarker(new google.maps.LatLng($("#location_lat").val(), $("#location_lon").val()));
			setTimeout(function(){map.setCenter(window.marker.getPosition());},100);
        }

		$('#fill-geo').click(function() {
			googleMapsControl.getGeoForAddress($('.field-title:first').val(), function(data) {
				if(window.marker) {
					removeMarker(window.marker);
				}

				addMarker(data.geometry.location);
				fillLatLng(data.geometry.location);

				google.maps.event.addListener(window.marker, 'dragend', function(e) {
					fillLatLng(window.marker.getPosition());
					map.setCenter(window.marker.getPosition());
				});

				google.maps.event.addListener(window.marker, 'drag', function(e) {
					fillLatLng(window.marker.getPosition());
				});
			});
		});

		google.maps.event.addListener(map, 'click', function(e)	{
			if(window.marker) {
				removeMarker(window.marker);
			}

			addMarker(e.latLng);

			google.maps.event.addListener(window.marker, 'click', function(e) {
				fillLatLng(window.marker.getPosition());
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
	}

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
