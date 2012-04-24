{# This template is lazy loaded from the _geomap_admin_location.tpl #}

{% with m.rsc[id].computed_location_lat as latitude %}
{% with m.rsc[id].computed_location_lng as longitude %}
<fieldset class="admin-form">
	<div class="row">
		<div class="control-group span4">
			<label for="location_lat" class="control-label">{_ Latitude _}</label>
			<div class="controls">
				<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="span2" />
				<p class="help-inline">{_ indexed _}: {{ latitude }}</p>
			</div>
		</div>

		<div class="control-group span4">
			<label for="location_lng" class="control-label">{_ Longitude _}</label>
			<div class="controls">
				<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="span2" />
				<p class="help-inline">{_ indexed _}: {{ longitude }}</p>
			</div>
		</div>
	</div>
	
	<div class="well">
		<button class="btn" id="location_me"><i class="icon-screenshot"></i> {_ Set to current location _}</button>
		<button class="btn" id="location_address"><i class="icon-screenshot"></i> {_ Set to entered address _}</button>
		<button class="btn" id="location_clear">{_ Clear _}</button>
		<button class="btn" id="location_reset">{_ Reset _}</button>
		<button class="btn btn-primary" id="location_reset">{_ Save this page _}</button>
	</div>
</fieldset>

<div id="{{ #geomap }}" class="admin-geomap" style="width: 720px; height: 480px;"></div>

<p class="help-inline">{_ Please click on the map to select the location. _}</p>

{% javascript %}
OpenLayers.ImgPath = '/lib/images/'

var map;
var map_location;
var markers;
var marker_icon;
var marker;
var map_geolocate;

setTimeout(function() {
    map = new OpenLayers.Map('{{ #geomap }}', {
        theme: '/lib/css/openlayers/theme/default/style.css'
    });

    var layer = new OpenLayers.Layer.OSM("OpenStreetMap");
    map.addLayer(layer);
    markers = new OpenLayers.Layer.Markers("{_ Markers _}");
    map.addLayer(markers);

    var marker_size = new OpenLayers.Size(21,25);
    var marker_offset = new OpenLayers.Pixel(-(marker_size.w/2), -marker_size.h);
    marker_icon = new OpenLayers.Icon('/lib/images/marker.png', marker_size, marker_offset);

    {# Center the map on the current (calculated) position #}
    {% if longitude|is_defined and latitude|is_defined %}
        var map_location = new OpenLayers
                            .LonLat({{longitude}}, {{latitude}})
                            .transform(
                                new OpenLayers.Projection("EPSG:4326"),
                                map.getProjectionObject());
        map.setCenter(map_location, 15);
        marker_icon.setOpacity(0.8);
        markers.addMarker(new OpenLayers.Marker(map_location, marker_icon));
    {% else %}
        var map_location = new OpenLayers.LonLat(0, 0);
        map.setCenter(map_location, 2);
    {% endif %}

    {# Add the geolocation API handler #}
    map_geolocate = new OpenLayers.Control.Geolocate({
        bind: false,
        geolocationOptions: {
            enableHighAccuracy: false,
            maximumAge: 0,
            timeout: 7000
        }
    });
    map.addControl(map_geolocate);

    map_geolocate.events.register("locationupdated",map_geolocate,function(e) {
        map_geolocate.deactivate();
        map_mark_location(e.position.coords.longitude, e.position.coords.latitude);
        $('#location_me').removeClass('disabled');
    });

    {# Add the click-on-map handler #}
    OpenLayers.Control.Click = OpenLayers.Class(OpenLayers.Control, {                
        defaultHandlerOptions: {
            'single': true,
            'double': false,
            'pixelTolerance': 0,
            'stopSingle': false,
            'stopDouble': false
        },

        initialize: function(options) {
            this.handlerOptions = OpenLayers.Util.extend(
                {}, this.defaultHandlerOptions
            );
            OpenLayers.Control.prototype.initialize.apply(
                this, arguments
            ); 
            this.handler = new OpenLayers.Handler.Click(
                this, {
                    'click': this.trigger
                }, this.handlerOptions
            );
        }, 

        trigger: function(e) {
            var lonlat = map.getLonLatFromViewPortPx(e.xy);
            lonlat.transform(new OpenLayers.Projection("EPSG:900913"), 
                             new OpenLayers.Projection("EPSG:4326"));
            map_mark_location(lonlat.lon, lonlat.lat, true);
        }

    });

    var click = new OpenLayers.Control.Click();
    map.addControl(click);
    click.activate();
}, 500);

$('#location_me').click(function(ev) {
    map_geolocate.activate();
    $(this).addClass('disabled');
    ev.preventDefault();
});

$('#location_address').click(function(ev) { 
    var args = {
        street: $('#address_street_1').val(),
        city: $('#address_city').val(),
        postcode: $('#address_postcode').val(),
        state: $('#address_state').val(),
        country: $('#address_country').val(),
        z_delegate: 'mod_geomap'
    };
    z_notify("address_lookup", args);
    $(this).addClass('disabled');
    ev.preventDefault(); 
});
$('#location_clear').click(function(ev) { 
    $('#location_lat').val('');
    $('#location_lng').val('');
    markers.clearMarkers();
    ev.preventDefault();
});
$('#location_reset').click(function(ev) { 
    $('#location_lat').val('{{ m.rsc[id].location_lat }}');
    $('#location_lng').val('{{ m.rsc[id].location_lng }}');
    var latitude = parseFloat('{{ m.rsc[id].computed_location_lat }}');
    var longitude = parseFloat('{{ m.rsc[id].computed_location_lng }}');
    if (latitude != NaN && longitude != NaN) {
        map_mark_location(longitude, latitude);
    }
    ev.preventDefault();
});

window.map_mark_location_error = function() {
    z_growl_add("{_ Could not find the location. _}");
    $('#location_address').removeClass('disabled');
}

window.map_mark_location = function(longitude, latitude, is_click) {
    longitude = Math.round(longitude*1000000) / 1000000;
    latitude = Math.round(latitude*1000000) / 1000000;

    markers.clearMarkers();
    var map_location = new OpenLayers
                        .LonLat(longitude, latitude)
                        .transform(
                            new OpenLayers.Projection("EPSG:4326"),
                            map.getProjectionObject());
    marker_icon.setOpacity(0.8);
    markers.addMarker(new OpenLayers.Marker(map_location, marker_icon));
    $('#location_lat').val(latitude.toString());
    $('#location_lng').val(longitude.toString());
    if (!is_click) {
        map.setCenter(map_location, 15);
        z_growl_add("{_ Location has been set. _}");
        $('#location_address').removeClass('disabled');
    }
}

{% endjavascript %}
{% endwith %}
{% endwith %}
