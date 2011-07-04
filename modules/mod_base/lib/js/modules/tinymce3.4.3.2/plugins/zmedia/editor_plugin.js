/**
 * $Id: editor_plugin_src.js 677 2008-03-07 13:52:41Z spocke $
 *
 * @author Moxiecode
 * @copyright Copyright © 2004-2008, Moxiecode Systems AB, All rights reserved.
 */


(function() {
	tinymce.create('tinymce.plugins.ZotonicMediaPlugin', {

        init : function(ed, url) {
                var self = this;
                
                // Register commands
                ed.addCommand('mceZotonicMedia', function() {
                        var n = ed.selection.getNode();

                        if (n && self._domIsZMedia(n)) 
                        {
                            // Open properties dialog
                            self.propertiesDialog(n);
                        } 
                        else {
                            // Open "insert" dialog
                            z_event('zmedia');
                            window.z_choose_zmedia = function(id) {
                                if (!id) return;
                                var res = self._zMediaHtml(id, {align: "block", size: "middle", crop: '', link: ''});
                                ed.execCommand('mceInsertContent', false, res['html'], {});
                            }
                        }

                    });

                ed.onLoadContent.add(function(ed, o) {
                    ed.setContent(self._zMarkersToMediaHtml(o.content));
                                         
                    ed.onPostProcess.add(function(ed, o) {
                        o.content = self._MediaHtmlToMarkers(o.content);
                        });

                    });

                ed.onSetContent.add(function(ed, o) {
                    });

            
            
                // Register buttons
                ed.addButton('zmedia', {
                    title : 'Insert a Zotonic media item.',
                            cmd : 'mceZotonicMedia',
                            'class': 'mce_image'
                            });
            },

            propertiesDialog: function(node) {
                var id = this._zMediaIdFromDOM(node);
                var opts = this._zMediaOptsFromDOM(node);

                $.dialogAdd({title: 'Media properties',
                            text: '<div class="z-tinymce-media-options"><img src="/admin/media/preview/' + id + '" class="z-tinymce-media-left" />' + 
                            '<p>Choose the properties of the image:</p>' + 

                            '<div class="form-item clearfix left">' +
                            '<label for="a-block"><input type="radio" name="align" ' + (opts.align=='block'?'checked="checked"':'') + ' value="block" id="a-block"> Between text</label>' +
                            '<label for="a-left"><input type="radio" name="align" ' + (opts.align=='left'?'checked="checked"':'') + 'value="left" id="a-left"> Aligned left</label>' +
                            '<label for="a-right"><input type="radio" name="align" ' + (opts.align=='right'?'checked="checked"':'') + 'value="right" id="a-right"> Aligned right</label>' +
                            '</div>' +

                            '<div class="form-item clearfix left" style="padding-left: 8px">' +
                            '<label for="a-small"><input type="radio" name="size" ' + (opts.size=='small'?'checked="checked"':'') + ' value="small" id="a-small"> Small</label>' +
                            '<label for="a-middle"><input type="radio" name="size" ' + (opts.size=='middle' || opts.size == undefined?'checked="checked"':'') + 'value="middle" id="a-middle"> Middle</label>' +
                            '<label for="a-large"><input type="radio" name="size" ' + (opts.size=='large'?'checked="checked"':'') + 'value="large" id="a-large"> Large</label>' +
                            '</div>' +

                            '<div class="form-item clearfix left" style="padding-left: 8px">' +
                            '<label for="a-crop"><input type="checkbox" name="crop" ' + (opts.crop=='crop'?'checked="checked"':'') + ' value="crop" id="a-crop"> Crop image</label>' +
                            '<label for="a-link"><input type="checkbox" name="link" ' + (opts.link=='link'?'checked="checked"':'') + ' value="crop" id="a-link"> Make link</label>' +
                            '<br/><br/>' +
                            '<button onclick="window.z_media_props();">Submit</button>' +
                            '<button onclick="$.dialogRemove($(\'.dialog\'));">Cancel</button>' +
                            '</div>' +

                            '</div>'});

                window.z_media_props = function() {
                    opts = {
                        'align': $('input[name="align"]:checked', $('.dialog')).val(),
                        'size': $('input[name="size"]:checked', $('.dialog')).val(),
                        'crop': $('input[name="crop"]:checked', $('.dialog')).val() ? "crop" : "",
                        'link': $('input[name="link"]:checked', $('.dialog')).val() ? "link" : ""
                    };

                    var cls = "z-tinymce-media ";
                    for (k in opts) {
                        cls += "z-tinymce-media-" + k + "-" + opts[k] + " ";
                    }
                    node.className = cls;
                    $.dialogRemove($('.dialog'));
                }
            },
                
            getInfo : function() {
                return {
                longname : 'Zotonic Media Plugin',
                        author : 'Arjan Scherpenisse',
                        authorurl : 'http://www.zotonic.com',
                        infourl : 'http://www.zotonic.com',
                        version : tinymce.majorVersion + "." + tinymce.minorVersion
                        };
            },
                
                
            // Private methods //

            _domIsZMedia: function(el) {
                return (el.id.substr(0, 8) == "z-media-");
            },

            _zMediaIdFromDOM: function(el) {
                return el.id.replace('z-media-', '');
            },

            _zMediaOptsFromDOM: function(el) {
                return this._zMediaOptsFromClassName(el.className);
            },

            _zMediaOptsFromClassName: function(cls) {
                var s = cls.split(" ");
                opts = {};
                for (var i in s) {
                    var m = (new RegExp('^z-tinymce-media-(.*?)-(.*)$')).exec(s[i]);
                    if (m) {
                        opts[m[1]] = m[2];
                    }
                }
                return opts;
            },

            _zMediaOptsToDOM: function(opts) {
                var cls = "z-tinymce-media ";
                for (k in opts) {
                    cls += "z-tinymce-media-" + k + "-" + opts[k] + " ";
                }
                return cls;
            },

            _zMediaOptsToRepr: function(opts) {
                var o = "{";
                for (k in opts) {
                    o += '"' + k + '": "' + opts[k] + '", ';
                }
                o = o.substr(0, o.length-2) + "}";
                return o;
            },

            _zMediaClass: function() {
                return "z-tinymce-media";
            },
        
            _zMediaId: function(id) {
                return "z-media-" + id;
            },

            _MediaHtmlToMarkers: function (html) {
                var re = new RegExp('<img.*?/>', 'g');
                var m;
                while ( (m = re.exec(html)) )
                {
                    var img = m[0];
                    var idmatch = (new RegExp('id="z-media-([0-9]+)', 'g')).exec(img);
                    if (!idmatch) return html;
                    var id = idmatch[1];
                    var cls = (new RegExp('class="(.*?)"', 'g')).exec(img)[1];
                    var opts = this._zMediaOptsFromClassName(cls);
                    var newtag = this._zMediaMarker(id, opts);

                    html = html.substr(0, re.lastIndex - img.length) + newtag + html.substr(re.lastIndex);
                    re.lastIndex = re.lastIndex - img.length + newtag.length;
                }
                return html;
            },

            _zMediaHtml: function(id, opts) {
                var divid = this._zMediaId(id);
                return {'html': '<img class="' + this._zMediaOptsToDOM(opts) + '" id="' +divid + '" src="/admin/media/preview/' + id + '" />', 'id': divid};
            },

            _zMediaMarker: function(id, opts) {
                return "<!-- z-media " + id + " " + this._zMediaOptsToRepr(opts) + " -->";
            },

            _zMarkersToMediaHtml: function (html) {
                var re = new RegExp('<!-- z-media (.*?) (.*?)-->', 'g');
                var m;
                while ( (m = re.exec(html)) )
                {
                    var id = m[1];
                    var opts = eval("(" + m[2] + ")");
                    var repl = this._zMediaHtml(id, opts);
                    html = html.substr(0, re.lastIndex - m[0].length) + repl['html'] + html.substr(re.lastIndex);
                    re.lastIndex = re.lastIndex - m[0].length + repl.length;
                }
                return html;
            }

        }

        );

	// Register plugin
	tinymce.PluginManager.add('zmedia', tinymce.plugins.ZotonicMediaPlugin);
 })();
