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
    
    /* Just show where we clicked */
    var display = function(event) {
        console.log(event);
        for(var i=0; i < event.feature.cluster.length; ++i) {
            console.log(event.feature.cluster[i].data);
        }
    }

    var GeoMap = {
        _map: null,
        _base: null
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
        this._map.setCenter(new OpenLayers.LonLat(0, 0), 1);
    };
    GeoMap.map = function() {
        return this._map;
    };
    GeoMap.addCollection = function(name, searchParams) {
        var style = new OpenLayers.Style({
            pointRadius: "${radius}",
            fillColor: "#ffcc66",
            fillOpacity: 0.8,
            strokeColor: "#cc6633",
            strokeWidth: 2,
            strokeOpacity: 0.8
        }, {
            context: {
                radius: function(feature) {
                    return Math.min(feature.attributes.count, 7) + 3;
                }
            }
        });
        searchParams.count = searchParams.count || 1000;
        var searchLayer = new OpenLayers.Layer.Vector(name, {
            projection: "EPSG:4326",
            strategies: [
                new OpenLayers.Strategy.Fixed(),
                new OpenLayers.Strategy.Cluster()
            ],
            protocol: new OpenLayers.Protocol.Script({
                url: "/api/geomap/locations",
                params: searchParams,
                callbackKey: 'jsonp',
                format: new OpenLayers.Format.ModGeoMap()
            }),
            styleMap: new OpenLayers.StyleMap({
                "default": style,
                "select": {
                    fillColor: "#8aeeef",
                    strokeColor: "#32a8a9"
                }
            })
        });
        this._map.addLayers([searchLayer]);
    };

    window.GeoMap = GeoMap;

/*
    var.style = new OpenLayers.Style({
        pointRadius: "${radius}",
        fillColor: "#ffcc66",
        fillOpacity: 0.8,
        strokeColor: "#cc6633",
        strokeWidth: 2,
        strokeOpacity: 0.8
    }, {
        context: {
            radius: function(feature) {
                return Math.min(feature.attributes.count, 7) + 3;
            }
        }
    });

    var people = new OpenLayers.Layer.Vector("People", {
        projection: "EPSG:4326",
        strategies: [
            new OpenLayers.Strategy.Fixed(),
            new OpenLayers.Strategy.Cluster()
        ],
        protocol: new OpenLayers.Protocol.Script({
            url: "/api/geomap/locations",
            params: {
                cat: 'person',
                count: 1000
            },
            callbackKey: 'jsonp',
            format: new OpenLayers.Format.ModGeoMap()
        }),
        styleMap: new OpenLayers.StyleMap({
            "default": style,
            "select": {
                fillColor: "#8aeeef",
                strokeColor: "#32a8a9"
            }
        })
    });

    var select = new OpenLayers.Control.SelectFeature(people, {click: true});

    map.addControl(select);
    select.activate();
    people.events.on({"featureselected": display});

    map.addLayers([base, people]);

    // template setup
    // template = new jugl.Template("template");
*/
}( window.jQuery );

