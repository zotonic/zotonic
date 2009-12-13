var googleMapsControl =
{
	defaults:
	{
		zoom: 5,
		disableDefaultUI: true,
		mapTypeId: google.maps.MapTypeId.ROADMAP,
		navigationControl: true,
		navigationControlOptions:
		{
			style: google.maps.NavigationControlStyle.SMALL
		}
	},
	
	opts: {},
	
	buildMap: function(options)
	{
		var self			= googleMapsControl;
		self.opts			= $.extend({}, self.defaults, options);
		self.opts.center 	= new google.maps.LatLng(self.opts.lat, self.opts.lng);
		self.map			= new google.maps.Map($('#'+self.opts.mapId)[0], self.opts);
	},

	getMap: function()
	{
		return googleMapsControl.map;
	},
	
	getMarker: function()
	{
		return googleMapsControl.marker;
	},
	
	addMarker: function(latLng, lat, lng, text, draggable)
	{
		latLng		= latLng || null;
		draggable	= draggable || false
		var self	= googleMapsControl;
		
		if(latLng)
		{
			var position = latLng;
		}
		else
		{
			var position = new google.maps.LatLng(lat, lng);
		}
		
		self.marker = new google.maps.Marker(
		{
			position:	position,
			map: 		self.map,
			title:		text,
			icon:		'../images/map_icon.png',
			flat: 		true,
			draggable:	draggable
		});
		
		google.maps.event.addListener(self.marker, 'click', function()
		{
			new InfoBox({latlng: self.marker.getPosition(), map: self.map, text: self.marker.getTitle()});
		});
	},
	
	addMarkerByAddress: function(address, title)
	{
		title = title || '';
		
		var self = googleMapsControl;
		
		self.geocoder = new google.maps.Geocoder();
		self.geocoder.geocode({'address': address, 'partialmatch': true}, geocodeResult);
		
		function geocodeResult(results, status)
		{
			if(status == 'OK' && results.length > 0) 
			{
				self.addMarker(results[0].geometry.location.lat(), results[0].geometry.location.lng(), title)
		    } 
			else 
			{
				throw("Geocode was not successful for the following reason: " + status);
		    }
		}
	},
	
	getGeoForAddress: function(address, callback)
	{
		var self = googleMapsControl;

		self.geocoder = new google.maps.Geocoder();
		self.geocoder.geocode({'address': address, 'partialmatch': true}, function(results, status)
		{
			if(status == 'OK' && results.length > 0) 
			{
				if(callback) callback(results[0]);
		    } 
			else 
			{
				alert("Geocode was not successful for the following reason: " + status);
		    }
		});
	},
	
	drawLine: function(coordinates)
	{
		var self = googleMapsControl;

		self.linePath = new google.maps.Polyline(
		{
			path: coordinates,
			strokeColor: "#000000",
			strokeOpacity: 0.5,
			strokeWeight: 5
		});

		self.linePath.setMap(self.map);
	}
}