/* geomap js
----------------------------------------------------------

@package:	Zotonic 2012	
@Author:	Marc Worrell <marc@worrell.nl>

Copyright 2012 Marc Worrell

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
 
http://www.apache.org/licenses/LICENSE-2.0
 
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

!function( $ ) {

    /* Connect to Zotonic */
    OpenLayers.ImgPath = '/lib/images/'
    OpenLayers.Format.ModGeoMap = OpenLayers.Class(OpenLayers.Format, {
        read: function(obj) {
            if (typeof obj.error == "object") {
                throw new Error(
                    ['ModGeoMap failure response (',
                     obj.error.code,
                     '): ',
                     obj.error.message].join(''));
            }
            var loc, point,
                feature, features = [];
        
            for(var i=0,l=obj.length; i < l; i++) {
                loc = obj[i];
                point = new OpenLayers.Geometry.Point(loc.location_lng, loc.location_lat);
                feature = new OpenLayers.Feature.Vector(point, loc);
                features.push(feature);
            }
            return features;
        }
    });
    
    var GeoMap = {
        _map: null,
        _base: null,
        _selectControl: null,
        _selectedFeature: null,
        _popup : null
    };
    
    GeoMap.init = function() {
        this._map = new OpenLayers.Map({
            div: 'map',
            theme: null,
            projection: new OpenLayers.Projection("EPSG:900913"),
            numZoomLevels: 12,
            controls: [
                new OpenLayers.Control.TouchNavigation({
                    dragPanOptions: {
                        enableKinetic: true
                    }
                }),
                new OpenLayers.Control.Navigation(),
                new OpenLayers.Control.KeyboardDefaults(),
                new OpenLayers.Control.Zoom()
            ]
        });

        this._base = new OpenLayers.Layer.OSM("OpenStreetMap", null, {
            transitionEffect: 'resize'
        });
        
        this._map.addLayers([this._base]);
        var center = new OpenLayers.LonLat(0, 30).transform(new OpenLayers.Projection("EPSG:4326"), GeoMap.map().getProjectionObject());
        this._map.setCenter(center, 3);
    };

    GeoMap.map = function() {
        return this._map;
    };
    GeoMap.selectControl = function() {
        return this._selectControl;
    }

    GeoMap.addCollections = function(list) {
        var self = this;
        var default_style = new OpenLayers.Style({
                pointRadius: "${radius}",
                fillColor: "#ffcc66",
                fillOpacity: 0.8,
                strokeColor: "#cc6633",
                strokeWidth: 2,
                strokeOpacity: 0.8
            }, {
                context: {
                    radius: function(feature) {
                        return Math.min(feature.attributes.count, 7) + 5;
                    }
                }
            });

        var layers = [];
        for (i=0; i<list.length; i++)
        {
            var c = list[i];
            c.query.count = c.query.count || 1000;
            var layer = new OpenLayers.Layer.Vector(c.name, {
                projection: "EPSG:4326",
                strategies: [
                    new OpenLayers.Strategy.Fixed(),
                    new OpenLayers.Strategy.Cluster()
                ],
                protocol: new OpenLayers.Protocol.Script({
                    url: "/api/geomap/locations",
                    params: c.query,
                    callbackKey: 'jsonp',
                    format: new OpenLayers.Format.ModGeoMap()
                }),
                styleMap: new OpenLayers.StyleMap({
                    "default": default_style,
                    "select": {
                        fillColor: "#8aeeef",
                        strokeColor: "#32a8a9"
                    }
                }),
                eventListeners: {
                    "featureselected": function(evt) { self.featureSelect(evt); },
                    "featureunselected": function(evt) { self.featureUnselect(evt); }
                }
            });
            layers.push(layer)
        }
        var selectFeatures = new OpenLayers.Control.SelectFeature(layers, { click: true });
        this._map.addLayers(layers);
        this._map.addControl(selectFeatures);
        this._selectControl = selectFeatures;
        selectFeatures.activate();
    };
    
    // Needed only for interaction, not for the display.
    GeoMap.featureSelect = function(evt) {
        this._selectedFeature = evt.feature;
        var self = this;
        var cluster = this._selectedFeature.cluster;
        var t = "";
        
        for (var i=0, len=cluster.length; i<Math.min(len,3); i++) {
            var d = cluster[i].data;
            t += "<h3><a href='"+d.page_url+"'>"+d.title+"</a></h3>"
                 + "<p>"+d.address_country
                 + ", " + d.address_city
                 + ", " + d.address_street_1
                 + "</p>";
        }
        if (cluster.length > 3) {
            t = t+"<p class='geomap-other'>And <span>"+(cluster.length-3)+"</span> other.</p>";
        }
        this._popup = new OpenLayers.Popup.FramedCloud("featurePopup",
                                 this._selectedFeature.geometry.getBounds().getCenterLonLat(),
                                 new OpenLayers.Size(100,100),
                                 t,
                                 null, true, 
                                 function(_closeEvt) {
                                     // 'this' is the popup.
                                     if (this.feature.layer) {
                                         self._selectControl.unselect(this.feature);
                                     } else { 
                                         this.destroy();
                                     }
                                 });
        this._selectedFeature.popup = this._popup;
        this._popup.feature = this._selectedFeature;
        GeoMap.map().addPopup(this._popup, true);
    }
    GeoMap.featureUnselect = function(evt) {
        this._selectedFeature = evt.feature;
        if (this._selectedFeature.popup) {
            this._popup.feature = null;
            this._map.removePopup(this._selectedFeature.popup);
            this._selectedFeature.popup.destroy();
            this._selectedFeature.popup = null;
        }
    };

    window.GeoMap = GeoMap;

}( window.jQuery );

