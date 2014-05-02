/**
 * Set the image options or select an image to insert.
 *
 * @author Arjan Scherpenisse <arjan@scherpenisse.net>
 * @author Moxiecode
 * @copyright 2010-2012 Arjan Scherpenisse <arjan@scherpenisse.net>
 * @copyright Copyright 2004-2008, Moxiecode Systems AB, All rights reserved.
 */


(function() {
     window.tinyMCEzMedia = {
        toHTML: function(id, opts) {
            var cls = "z-tinymce-media ";
            cls += "z-tinymce-media-align-" + (opts.align || 'block');
            var html = '<img class="'
                + cls + '" '
                + 'data-zmedia-id="' +id + '" '
                + 'data-zmedia-opts="' + JSON.stringify(opts).replace(/\"/g, '&quot;') + '" '
                + ' src="/admin/media/preview/' + id + '" />';
            return html;
        }
    };

	tinymce.create('tinymce.plugins.ZotonicMediaPlugin', {
        init : function(ed, url) {
                var self = this;
                
                // Register commands
                ed.addCommand('mceZotonicMedia', function() {
                    var n = ed.selection.getNode();
                    if (n && self._domIsZMedia(n)) {
                        // Open properties dialog
                        self.propertiesDialog(n);
                    } else {
                        // Open "insert" dialog
                        window.z_choose_zmedia = function(id) {
                            if (!id) return;
                            var html = window.tinyMCEzMedia.toHTML(id, {align: "block", size: "middle", crop: '', link: ''});
                            ed.execCommand('mceInsertContent', false, html, {});
                        };
                        z_event('zmedia', {language: window.zEditLanguage(), is_zmedia: 1});
                    }
                });

                ed.onPostProcess.add(function(ed, o) {
                    o.content = self._MediaHtmlToMarkers(o.content);
                });
            
                ed.onLoadContent.add(function(ed, o) {
                    ed.setContent(self._zMarkersToMediaHtml(o.content));
                });

                ed.onBeforeSetContent.add(function(ed, o) {
                    o.content = self._zMarkersToMediaHtml(o.content);
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

                z_dialog_open({
                    title: 'Media properties',
                    text: 
 '<form id="zmedia-props-form" class="form">'
+'  <div class="row">'
+'      <div class="span3">'
+'          <img src="/admin/media/preview/' + id + '" class="z-tinymce-media-left" />'
+'      </div>' 
+'      <div class="span6">'
+'          <div class="row">'
+'              <div class="span3">'
+'                  <div class="control-group">'
+'                      <label class="control-label">Alignment</label>'
+'                      <div class="controls">'
+'                          <label class="radio"><input type="radio" name="align" ' + (opts.align=='block'?'checked="checked"':'') + ' value="block" id="a-block"> Between text</label>'
+'                          <label class="radio"><input type="radio" name="align" ' + (opts.align=='left'?'checked="checked"':'') + 'value="left" id="a-left"> Aligned left</label>'
+'                          <label class="radio"><input type="radio" name="align" ' + (opts.align=='right'?'checked="checked"':'') + 'value="right" id="a-right"> Aligned right</label>'
+'                      </div>'
+'                  </div>'
+'              </div>'
+'              <div class="span3">'
+'                  <div class="control-group">'
+'                      <label class="control-label">Size</label>'
+'                      <div class="controls">'
+'                          <label class="radio"><input type="radio" name="size" ' + (opts.size=='small'?'checked="checked"':'') + ' value="small" id="a-small"> Small</label>'
+'                          <label class="radio"><input type="radio" name="size" ' + (opts.size=='middle' || opts.size == undefined?'checked="checked"':'') + 'value="middle" id="a-middle"> Middle</label>'
+'                          <label class="radio"><input type="radio" name="size" ' + (opts.size=='large'?'checked="checked"':'') + 'value="large" id="a-large"> Large</label>'
+'                      </div>'
+'                  </div>'
+'              </div>'
+'          </div>'
+'          <div class="row">'
+'              <div class="span3">'
+'                  <div class="control-group">'
+'                      <label class="control-label">Crop</label>'
+'                      <div class="controls">'
+'                          <label class="checkbox"><input type="checkbox" name="crop" ' + (opts.crop=='crop'?'checked="checked"':'') + ' value="crop" id="a-crop"> Crop image</label>'
+'                      </div>'
+'                  </div>'
+'              </div>' 
+'              <div class="span3">'
+'                  <div class="control-group">'
+'                      <label class="control-label">Link</label>'
+'                      <div class="controls">'
+'                          <label class="checkbox"><input type="checkbox" name="link" ' + (opts.link=='link'?'checked="checked"':'') + ' value="crop" id="a-link"> Make link</label>'
+'                      </div>'
+'                  </div>'
+'              </div>'
+'          </div>'
+'      </div>' 
+'  </div>'
+'  <div class="modal-footer">'
+'      <button class="btn" id="zmedia-props-cancel">Cancel</button>'
+'      <button class="btn btn-primary" type="submit">Save Properties</button>'
+'  </div>'
+'</form>'
                });
                
                $('#zmedia-props-form').submit(function() {
                    var new_opts = {
                        'align': $('#zmedia-props-form input[name="align"]:checked').val(),
                        'size': $('#zmedia-props-form input[name="size"]:checked').val(),
                        'crop': $('#zmedia-props-form input[name="crop"]:checked').val() ? "crop" : "",
                        'link': $('#zmedia-props-form input[name="link"]:checked').val() ? "link" : ""
                    };
                    var el = $(window.tinyMCEzMedia.toHTML(id, new_opts));
                    node.className = el.attr("class");
                    $(node)
                        .attr("data-zmedia-opts", el.attr("data-zmedia-opts"))
                        .attr("data-zmedia-id", el.attr("data-zmedia-id"));
                    z_dialog_close();
                    return false;
                });
                $('#zmedia-props-cancel').click(function() { 
                    z_dialog_close(); 
                    return false;
                });
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
                return this._zMediaIdFromDOM(el) !== null;
            },

            _zMediaIdFromDOM: function(el) {
                return el.getAttribute('data-zmedia-id');
            },

            _zMediaOptsFromDOM: function(el) {
                return eval("(" + el.getAttribute("data-zmedia-opts") + ")");
            },

            _zMediaClass: function() {
                return "z-tinymce-media";
            },
        
            _MediaHtmlToMarkers: function (html) {
                var re = new RegExp('<img.*?/>', 'g');
                var m;
                while ( (m = re.exec(html)) )
                {
                    var img = m[0];
                    var idmatch = (new RegExp('data-zmedia-id="([0-9]+)', 'g')).exec(img);
                    if (!idmatch) return html;
                    var id = idmatch[1];
                    var optsmatch = (new RegExp('data-zmedia-opts="(\\{.*?\\})"', 'g')).exec(img);
                    if (!optsmatch) return html;
                    var opts = eval("(" + optsmatch[1].replace(/&quot;/g, '"') + ")");
                    var newtag = this._zMediaMarker(id, opts);

                    html = html.substr(0, re.lastIndex - img.length) + newtag + html.substr(re.lastIndex);
                    re.lastIndex = re.lastIndex - img.length + newtag.length;
                }
                return html;
            },

            _zMediaMarker: function(id, opts) {
                return "<!-- z-media " + id + " " + JSON.stringify(opts) + " -->";
            },

            _zMarkersToMediaHtml: function (html) {
                var re = new RegExp('<!-- z-media (.*?) (.*?)-->', 'g');
                var m;
                while (m = re.exec(html)) {
                    var id = m[1];
                    var opts = eval("(" + m[2] + ")");
                    var part = window.tinyMCEzMedia.toHTML(id, opts);
                    html = html.substr(0, re.lastIndex - m[0].length) + part + html.substr(re.lastIndex);
                    re.lastIndex = re.lastIndex - m[0].length + part.length;
                }
                return html;
            }
        }
    );

	// Register plugin
	tinymce.PluginManager.add('zmedia', tinymce.plugins.ZotonicMediaPlugin);
 })();
