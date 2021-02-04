/* adminwidget js
----------------------------------------------------------

@package:   Zotonic 2009, 2012
@Author:    Tim Benniks <tim@timbenniks.nl>

Copyright 2009 Tim Benniks
Copyright 2012 Arjan Scherpenisse

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

$.widget("z.adminwidget",
{
    _init: function()
    {
        var self = this;
        self.element.addClass("widget-active");
        self.item = self.element.find("div.widget-content");
        self.header = self.element.find(".widget-header");
        if (self.header.length === 0) {
            self.header = self.element.find(".language-tabs");
        }
        self.tools = self.header.find(".widget-header-tools");
        if (self.tools.length == 0) {
            self.tools = $("<div>").appendTo(self.header).addClass("widget-header-tools");
        }
        self.tabs = self.element.find(".language-tabs");
        var doMinify = self.options.minifier || $(self.element).attr("data-minifier");
        if (doMinify) {
            self.icon = $("<i>").appendTo(self.tools).css("cursor", "pointer");
            self.header
                .on("mouseover", function(){self.icon.addClass('white');})
                .on("mouseout", function(){self.icon.removeClass('white');})
                .attr("title", z_translate("Click to toggle"))
                .click(function(ev){self.toggle(ev);});

            var id = self.element.attr("id");
            cotonic.broker.call("model/sessionStorage/get/adminwidget-"+id)
                .then(function(msg) {
                    var minified = self.options.minifiedOnInit && self.options.minifier;
                    if (typeof msg.payload == 'boolean') {
                        minified = !msg.payload;
                    }
                    self.setVisible(!minified, true);
                });
            cotonic.broker.subscribe("model/sessionStorage/event/adminwidget-"+id,
                function(msg) {
                    self.setVisible(!!msg.payload);
                });
        }
    },

    toggle: function(ev) {
        if (    $(ev.target).hasClass('widget-header')
            ||  $(ev.target).hasClass('z-icon-plus')
            ||  $(ev.target).hasClass('z-icon-minus')) {
            var self = this;
            var id = self.element.attr("id");
            cotonic.broker.publish("model/sessionStorage/post/adminwidget-"+id, !self.showing);
            ev.stopPropagation();
        }
    },

    setVisible: function(v, skipAnim) {
        var self = this;
        v ? self.show(skipAnim) : self.hide(skipAnim);
    },

    hide: function(skipAnim) {
        var self = this;
        if (skipAnim) {
            self.item.hide();
        } else {
            self.item.slideUp(200);
        }
        if (self.tabs) {
            self.tabs.hide();
        }
        self.icon.attr("class", "z-icon z-icon-plus");
        self.element.removeClass("widget-expanded");
        self.showing = false;
    },

    show: function(skipAnim) {
        var self = this;
        // only show items that have content
        self.item.each(function(index) {
            var $item = $(this);
            if (!self.itemIsEmpty($item)) {
                if (skipAnim) {
                    $item.show();
                } else {
                    $item.slideDown(200);
                }
            }
        });
        
        if (self.tabs) {
            self.tabs.show();
        }
        if (self.icon) {
            self.icon.attr("class", "z-icon z-icon-minus");
        }
        self.element.addClass("widget-expanded");
        self.showing = true;
    },
    
    itemIsEmpty: function(el) {
        return !$.trim(el.html());
    }
});

$.z.adminwidget.defaults = {
    minifiedOnInit: false,
    minifier: true
};
