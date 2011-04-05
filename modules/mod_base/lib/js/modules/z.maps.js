(function($)
{
    window.googleMapsControl =
    {
	    defaults:
	    {
		    zoom: 2,
		    disableDefaultUI: true,
		    mapTypeId: google.maps.MapTypeId.HYBRID,
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

		    self.fluster = new Fluster2(self.map, false);

		    self.fluster.styles =
		        {
			        0:
			        {
				        image: '/lib/images/map_icon_cluster.png',
				        textColor: '#000',
				        width: 23,
				        height: 27
			        },

			        10:
			        {
				        image: '/lib/images/map_icon_cluster.png',
				        textColor: '#000',
				        width: 23,
				height: 27
			        },

			        20:
			        {
				        image: './lib/images/map_icon_cluster.png',
				        textColor: '#000',
				        width: 23,
				        height: 27
			        }
		        };

		    self.fluster.initialize();
	    },

	    getMap: function()
	    {
		    return googleMapsControl.map;
	    },

	    getMarker: function()
	    {
		    return googleMapsControl.marker;
	    },

	    addMarker: function(lat, lng, text)
	    {
		    var self = googleMapsControl;

		    var marker = new google.maps.Marker(
		        {
			        position:	new google.maps.LatLng(lat, lng),
			        map: 		self.map,
			        title:		text,
			        icon:		'/lib/images/map_icon.png'
		        });

		    self.fluster.addMarker(marker);

		    google.maps.event.addListener(marker, 'click', function()
		                                  {
			                                  new InfoBox({latlng: marker.getPosition(), map: self.map, text: marker.getTitle()});
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
			        strokeColor: "#fff",
			        strokeOpacity: 0.5,
			        strokeWeight: 3
		        });

		    self.linePath.setMap(self.map);
	    }
    }
})(jQuery);
/*
 * Fluster2 0.1.0
 * Copyright (C) 2009 Fusonic GmbH
 *
 * This file is part of Fluster2.
 *
 * Fluster2 is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Fluster2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Creates a new Fluster to manage masses of markers in a Google Maps v3.
 *
 * @constructor
 * @param {google.maps.Map} the Google Map v3
 * @param {bool} run in debug mode or not
 */
function Fluster2(_map, _debug)
{
	// Private variables
	var map = _map;
	var projection = new Fluster2ProjectionOverlay(map);
	var me = this;
	var clusters = {};

	// Properties
	this.debugEnabled = _debug;
	this.gridSize = 60;
	this.markers = [];
	this.currentZoomLevel = -1;
	this.styles = {
		0: {
			image: 'http://gmaps-utility-library.googlecode.com/svn/trunk/markerclusterer/1.0/images/m1.png',
			textColor: '#FFFFFF',
			width: 53,
			height: 52
		},
		10: {
			image: 'http://gmaps-utility-library.googlecode.com/svn/trunk/markerclusterer/1.0/images/m2.png',
			textColor: '#FFFFFF',
			width: 56,
			height: 55
		},
		20: {
			image: 'http://gmaps-utility-library.googlecode.com/svn/trunk/markerclusterer/1.0/images/m3.png',
			textColor: '#FFFFFF',
			width: 66,
			height: 65
		}
	};

	// Timeouts
	var zoomChangedTimeout = null;

	/**
	 * Create clusters for the current zoom level and assign markers.
	 */
	function createClusters()
	{
		var zoom = map.getZoom();

		if(clusters[zoom])
		{
			me.debug('Clusters for zoom level ' + zoom + ' already initialized.');
		}
		else
		{
			// Create clusters array
			clusters[zoom] = [];

			// Walk all markers
			for(var i = 0; i < me.markers.length; i++)
			{
				var marker = me.markers[i];
				var done = false;

				// Find a cluster which contains the marker
				for(var j = clusters[zoom].length - 1; j >= 0; j--)
				{
					var cluster = clusters[zoom][j];
					if(cluster.contains(marker.getPosition()))
					{
						cluster.addMarker(marker);
						done = true;
						break;
					}
				}

				if(!done)
				{
					// No cluster found, create a new one
					var cluster = new Fluster2Cluster(me, marker);
					clusters[zoom].push(cluster);
				}
			}

			me.debug('Initialized ' + clusters[zoom].length + ' clusters for zoom level ' + zoom + '.');
		}

		// Hide markers of previous zoom level
		if(clusters[me.currentZoomLevel])
		{
			for(var i = 0; i < clusters[me.currentZoomLevel].length; i++)
			{
				clusters[me.currentZoomLevel][i].hide();
			}
		}

		// Show markers of current zoom level
		for(var i = 0; i < clusters[zoom].length; i++)
		{
			clusters[zoom][i].show();
		}

		// Set current zoom level
		me.currentZoomLevel = zoom;
	}

	/**
	 * Callback which is executed 500ms after the map's zoom level has changed.
	 */
	this.zoomChanged = function()
	{
		window.clearInterval(zoomChangedTimeout);
		zoomChangedTimeout = window.setTimeout(createClusters, 500);
	}

	/**
	 * Returns the map assigned to this Fluster.
	 */
	this.getMap = function()
	{
		return map;
	}

	/**
	 * Returns the map projection.
	 */
	this.getProjection = function()
	{
		return projection.getP();
	}

	/**
	 * Prints debug messages to console if debugging is enabled.
	 */
	this.debug = function(message)
	{
		if(me.debugEnabled)
		{
			console.log('Fluster2: ' + message);
		}
	}

	/**
	 * Adds a marker to the Fluster.
	 */
	this.addMarker = function(_marker)
	{
		me.markers.push(_marker);
	}

	/**
	 * Returns the currently assigned styles.
	 */
	this.getStyles = function()
	{
		return me.styles;
	}

	/**
	 * Sets map event handlers and setup's the markers for the current
	 * map state.
	 */
	this.initialize = function()
	{
		// Add event listeners
		google.maps.event.addListener(map, 'zoom_changed', this.zoomChanged);

		// Setup markers for the current state
		window.setTimeout(createClusters, 1000);
	}
}

/**
 * Cluster which holds one or more markers of the map.
 *
 * @constructor
 * @private
 * @param {Fluster2} the Fluster2 itself
 * @param {google.maps.Marker} the first marker
 */
function Fluster2Cluster(_fluster, _marker)
{
	// Properties
	this.fluster = _fluster;
	this.markers = [];
	this.bounds = null;
	this.marker = null;
	this.lngSum = 0;
	this.latSum = 0;
	this.center = _marker.getPosition();

	var me = this;

	// Calculate bounds
	var position = _fluster.getProjection().fromLatLngToDivPixel(_marker.getPosition());
	var positionSW = new google.maps.Point(
		position.x - _fluster.gridSize,
		position.y + _fluster.gridSize
	);
	var positionNE = new google.maps.Point(
		position.x + _fluster.gridSize,
		position.y - _fluster.gridSize
	);
	this.bounds = new google.maps.LatLngBounds(
		_fluster.getProjection().fromDivPixelToLatLng(positionSW),
		_fluster.getProjection().fromDivPixelToLatLng(positionNE)
	);

	/**
	 * Adds a marker to the cluster.
	 */
	this.addMarker = function(_marker)
	{
		this.markers.push(_marker);
	}

	/**
	 * Shows either the only marker or a cluster marker instead.
	 */
	this.show = function()
	{
		// Show marker if there is only 1
		if(this.markers.length == 1)
		{
			this.markers[0].setVisible(true);
		}
		else if(this.markers.length > 1)
		{
			// Hide all markers
			for(var i = 0; i < this.markers.length; i++)
			{
				this.markers[i].setVisible(false);
			}

			// Create marker
			if(this.marker == null)
			{
				this.marker = new Fluster2ClusterMarker(this.fluster, this);

				google.maps.event.addListener(this.marker, 'mouseover', me.debugShowMarkers);
				google.maps.event.addListener(this.marker, 'mouseout', me.debugHideMarkers);
			}

			// Show marker
			this.marker.show();
		}
	}

	/**
	 * Hides the cluster
	 */
	this.hide = function()
	{
		if(this.marker != null)
		{
			this.marker.hide();
		}
	}

	/**
	 * Shows all markers included by this cluster (debugging only).
	 */
	this.debugShowMarkers = function()
	{
		for(var i = 0; i < me.markers.length; i++)
		{
			me.markers[i].setVisible(true);
		}
	}

	/**
	 * Hides all markers included by this cluster (debugging only).
	 */
	this.debugHideMarkers = function()
	{
		for(var i = 0; i < me.markers.length; i++)
		{
			me.markers[i].setVisible(false);
		}
	}

	/**
	 * Returns the number of markers in this cluster.
	 */
	this.getMarkerCount = function()
	{
		return this.markers.length;
	}

	/**
	 * Checks if the cluster bounds contains the given position.
	 */
	this.contains = function(_position)
	{
		return me.bounds.contains(_position);
	}

	this.getPosition = function()
	{
		return this.markers[0].getPosition();
	}

	this.getBounds = function()
	{
		return this.bounds;
	}

	// Add the first marker
	this.addMarker(_marker);
}

/**
 * A cluster marker which shows a background image and the marker count
 * of the assigned cluster.
 *
 * @constructor
 * @private
 * @param {Fluster2} the Fluster2 itself
 * @param {Fluster2Cluster} the Fluster2Cluster assigned to this marker
 */
function Fluster2ClusterMarker(_fluster, _cluster)
{
	this.fluster = _fluster;
	this.cluster = _cluster;
	this.position = this.cluster.getPosition();
	this.markerCount = this.cluster.getMarkerCount();
	this.map = this.fluster.getMap();
	this.style = null;
	this.div = null;

	// Assign style
	var styles = this.fluster.getStyles();
	for(var i in styles)
	{
		if(this.markerCount > i)
		{
			this.style = styles[i];
		}
		else
		{
			break;
		}
	}

	// Basics
	google.maps.OverlayView.call(this);
	this.setMap(this.map);

	// Draw
	this.draw();
};

Fluster2ClusterMarker.prototype = new google.maps.OverlayView();

Fluster2ClusterMarker.prototype.draw = function()
{
	if(this.div == null)
	{
		var me = this;

		// Create div
		this.div = document.createElement('div');

		// Set styles
		this.div.style.position = 'absolute';
		this.div.style.width = this.style.width + 'px';
		this.div.style.height = this.style.height + 'px';
		this.div.style.lineHeight = 23 + 'px';
		this.div.style.background = 'transparent url("' + this.style.image + '") 50% 50% no-repeat';
		this.div.style.color = this.style.textColor;

		// Marker count
		this.div.style.textAlign = 'center';
		this.div.style.fontFamily = 'Arial, Helvetica';
		this.div.style.fontSize = '11px';
		this.div.style.fontWeight = 'bold';
		this.div.style.zIndex = 9999;
		this.div.innerHTML = this.markerCount;

		// Cursor and onlick
		this.div.style.cursor = 'pointer';
		google.maps.event.addDomListener(this.div, 'click', function() {
			me.map.fitBounds(me.cluster.getBounds());
		});

		this.getPanes().overlayLayer.appendChild(this.div);
	}

	// Position
	var position = this.getProjection().fromLatLngToDivPixel(this.position);
	this.div.style.left = (position.x - parseInt(this.style.width / 2)) + 'px';
	this.div.style.top = (position.y - parseInt(this.style.height / 2)) + 'px';
};

Fluster2ClusterMarker.prototype.hide = function()
{
	// Hide div
	this.div.style.display = 'none';
};

Fluster2ClusterMarker.prototype.show = function()
{
	// Show div
	this.div.style.display = 'block';
};

/**
 * An empty overlay which is used to retrieve the map projection panes.
 *
 * @constructor
 * @private
 * @param {google.maps.Map} the Google Maps v3
 */
function Fluster2ProjectionOverlay(map)
{
	google.maps.OverlayView.call(this);
	this.setMap(map);

	this.getP = function()
	{
		return this.getProjection();
	}
}

Fluster2ProjectionOverlay.prototype = new google.maps.OverlayView();

Fluster2ProjectionOverlay.prototype.draw = function()
{
}















/* An InfoBox is like an info window, but it displays
* under the marker, opens quicker, and has flexible styling.
* @param {GLatLng} latlng Point to place bar at
* @param {Map} map The map on which to display this InfoBox.
* @param {Object} opts Passes configuration options - content,
*   offsetVertical, offsetHorizontal, className, height, width
*/
function InfoBox(opts)
{
	google.maps.OverlayView.call(this);
	this.latlng_ = opts.latlng;
	this.map_ = opts.map;
	this.offsetVertical_ = -50;
	this.offsetHorizontal_ = -80;
	this.height_ = 'auto';
	this.width_ = 160;
	this.text = opts.text;

	var me = this;
	this.boundsChangedListener_ =
	google.maps.event.addListener(this.map_, "bounds_changed", function()
	{
		return me.panMap.apply(me);
	});

	// Once the properties of this OverlayView are initialized, set its map so
	// that we can display it.  This will trigger calls to panes_changed and
	// draw.
	this.setMap(this.map_);
}

/* InfoBox extends GOverlay class from the Google Maps API */
InfoBox.prototype = new google.maps.OverlayView();

/* Creates the DIV representing this InfoBox */
InfoBox.prototype.remove = function()
{
	if (this.div_)
	{
		this.div_.parentNode.removeChild(this.div_);
		this.div_ = null;
	}
};

/* Redraw the Bar based on the current projection and zoom level */
InfoBox.prototype.draw = function()
{
	// Creates the element if it doesn't exist already.
	this.createElement();
	if (!this.div_) return;

	// Calculate the DIV coordinates of two opposite corners of our bounds to
	// get the size and position of our Bar
	var pixPosition = this.getProjection().fromLatLngToDivPixel(this.latlng_);
	if (!pixPosition) return;

	// Now position our DIV based on the DIV coordinates of our bounds
	this.div_.style.width = this.width_ + "px";
	this.div_.style.left = (pixPosition.x + this.offsetHorizontal_) + "px";
	this.div_.style.height = this.height_ + "px";
	this.div_.style.top = (pixPosition.y + this.offsetVertical_ - $(this.div_).height()) + "px";
	this.div_.style.display = 'block';
};

/* Creates the DIV representing this InfoBox in the floatPane.  If the panes
* object, retrieved by calling getPanes, is null, remove the element from the
* DOM.  If the div exists, but its parent is not the floatPane, move the div
* to the new pane.
* Called from within draw.  Alternatively, this can be called specifically on
* a panes_changed event.
*/
InfoBox.prototype.createElement = function()
{
	var panes = this.getPanes();
	var div = this.div_;
	var self = this;

	if (!div)
	{
		// This does not handle changing panes. You can set the map to be null and
		// then reset the map to move the div.
		div = this.div_ = document.createElement("div");
		$(div).css(
		{
			position: 'absolute',
			width: self.width,
			height: self.height
		})
		.addClass('maps-dialog');

		var contentDiv = $("<div />").addClass('maps-dialog-content');
		contentDiv.html(self.text + ' <span class="maps-dialog-close"><a href="javascript:void(0);">close</a></span>');

		var closer = $('.maps-dialog-close', contentDiv)[0];

		function removeInfoBox(ib)
		{
			return function()
			{
				ib.setMap(null);
			};
		}

		google.maps.event.addDomListener(closer, 'click', removeInfoBox(this));

		$(div).append(contentDiv);
		$(div).hide();
		panes.floatPane.appendChild($(div)[0]);
		this.panMap();
	}
	else if (div.parentNode != panes.floatPane)
	{
		// The panes have changed.  Move the div.
		div.parentNode.removeChild(div);
		panes.floatPane.appendChild(div);
	}
	else
	{
		// The panes have not changed, so no need to create or move the div.
	}
}

/* Pan the map to fit the InfoBox. */
InfoBox.prototype.panMap = function()
{
	// if we go beyond map, pan map
	var map = this.map_;
	var bounds = map.getBounds();
	if (!bounds) return;

	// The position of the infowindow
	var position = this.latlng_;

	// The dimension of the infowindow
	var iwWidth = this.width_;
	var iwHeight = this.height_;

	// The offset position of the infowindow
	var iwOffsetX = this.offsetHorizontal_;
	var iwOffsetY = this.offsetVertical_;

	// Padding on the infowindow
	var padX = 40;
	var padY = 40;

	// The degrees per pixel
	var mapDiv = map.getDiv();
	var mapWidth = mapDiv.offsetWidth;
	var mapHeight = mapDiv.offsetHeight;
	var boundsSpan = bounds.toSpan();
	var longSpan = boundsSpan.lng();
	var latSpan = boundsSpan.lat();
	var degPixelX = longSpan / mapWidth;
	var degPixelY = latSpan / mapHeight;

	// The bounds of the map
	var mapWestLng = bounds.getSouthWest().lng();
	var mapEastLng = bounds.getNorthEast().lng();
	var mapNorthLat = bounds.getNorthEast().lat();
	var mapSouthLat = bounds.getSouthWest().lat();

	// The bounds of the infowindow
	var iwWestLng = position.lng() + (iwOffsetX - padX) * degPixelX;
	var iwEastLng = position.lng() + (iwOffsetX + iwWidth + padX) * degPixelX;
	var iwNorthLat = position.lat() - (iwOffsetY - padY) * degPixelY;
	var iwSouthLat = position.lat() - (iwOffsetY + iwHeight + padY) * degPixelY;

	// calculate center shift
	var shiftLng =
	(iwWestLng < mapWestLng ? mapWestLng - iwWestLng : 0) +
	(iwEastLng > mapEastLng ? mapEastLng - iwEastLng : 0);
	var shiftLat =
	(iwNorthLat > mapNorthLat ? mapNorthLat - iwNorthLat : 0) +
	(iwSouthLat < mapSouthLat ? mapSouthLat - iwSouthLat : 0);

	// The center of the map
	var center = map.getCenter();

	// The new map center
	var centerX = center.lng() - shiftLng;
	var centerY = center.lat() - shiftLat;

	// center the map to the new shifted center
	map.setCenter(new google.maps.LatLng(centerY, centerX));

	// Remove the listener after panning is complete.
	google.maps.event.removeListener(this.boundsChangedListener_);
	this.boundsChangedListener_ = null;
};