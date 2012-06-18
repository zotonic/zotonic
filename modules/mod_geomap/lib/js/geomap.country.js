/* geomap country js
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
    
    var GeoMapCountry = {
        _options: {},
        _map: null,
        _base: null,
        _selectControl: null,
        _selectedFeature: null,
        _popup : null,
        _popup_callback: null
    };
    
    GeoMapCountry.init = function(options) {
        this._options = options || {};
        this._map = new OpenLayers.Map({
            div: 'map',
            theme: null,
            projection: new OpenLayers.Projection("EPSG:4326"),
            numZoomLevels: 6,
            controls: [
                new OpenLayers.Control.TouchNavigation({
                    dragPanOptions: {
                        enableKinetic: true
                    }
                }),
                new OpenLayers.Control.Navigation(),
                new OpenLayers.Control.KeyboardDefaults(),
                new OpenLayers.Control.Zoom(),
                new OpenLayers.Control.Attribution()
            ]
        });
    };

    GeoMapCountry.addDataSet = function(title, dataSource, callback) {
        this._popup_callback = callback;
        var self = this;
        var style = new OpenLayers.Style({
                            fillOpacity: 0.9,
                            strokeColor: "#555555",
                            strokeWidth: 1,
                            fillColor: "${color}"
                        },
                        {
                            context: {
                                color: function(feature) {
                                    return feature.attributes["colour"];
                                }
                            }
                        });

        // Create polygon layer as vector features
        // http://dev.openlayers.org/docs/files/OpenLayers/Layer/Vector-js.html
        var mapopts = {
            protocol: dataSource,
            transitionEffect: 'resize',
            strategies: [
                new OpenLayers.Strategy.Fixed()
            ],
            styleMap: new OpenLayers.StyleMap({'default': style}),
            isBaseLayer: true,
            eventListeners: {
                "featureselected": function(evt) { self.featureSelect(evt); },
                "featureunselected": function(evt) { self.featureUnselect(evt); }
            }
        };
        if (typeof this._options.attribution == "string" && this._options.attribution.length > 0) {
            mapopts.attribution = this._options.attribution;
        }
        this._base = new OpenLayers.Layer.Vector("GML", mapopts);

        var selectFeatures = new OpenLayers.Control.SelectFeature([this._base], { click: true });
        this._map.addLayers([this._base]);
        this._map.addControl(selectFeatures);
        this._selectControl = selectFeatures;
        selectFeatures.activate();

        var center = new OpenLayers.LonLat(0, 30).transform(new OpenLayers.Projection("EPSG:4326"), this._map.getProjectionObject());
        this._map.setCenter(center, 2);
    };

    GeoMapCountry.map = function() {
        return this._map;
    };
    GeoMapCountry.selectControl = function() {
        return this._selectControl;
    }

    // Needed only for interaction, not for the display.
    GeoMapCountry.featureSelect = function(evt) {
        this._selectedFeature = evt.feature;
        var self = this;
        var data = evt.feature.data;
        var t = "<div id='geo-popup-info'><h2>"+data.name+"</h2><p>"+data.value+"</p></div>";
        
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
        this._popup.maxSize = new OpenLayers.Size(
                                        Math.min(jQuery(window).width(), 500),
                                        Math.min(jQuery(window).height()-100, 500));
        GeoMapCountry.map().addPopup(this._popup, true);
        if (self._popup_callback) {
            setTimeout(function() { self._popup_callback.apply(self, ['geo-popup-info', data]); }, 10);
        } else {
            setTimeout(function() {
                if (typeof data.rsc_id != "undefined") {
                    z_notify("render-update", {
                                template: "_geomap_popup.tpl",
                                id: data.rsc_id, 
                                name: data.name, 
                                color: data.colour, 
                                value: data.value, 
                                z_target_id: 'geo-popup-info'
                            });
                }
            }, 10);
        }
    }
    GeoMapCountry.featureUnselect = function(evt) {
        this._selectedFeature = evt.feature;
        if (this._selectedFeature.popup) {
            this._popup.feature = null;
            this._map.removePopup(this._selectedFeature.popup);
            this._selectedFeature.popup.destroy();
            this._selectedFeature.popup = null;
        }
    };

    window.GeoMapCountry = GeoMapCountry;

}( window.jQuery );

