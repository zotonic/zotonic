/* adminwidget js
----------------------------------------------------------

@package:	Zotonic 2009, 2012
@Author: 	Tim Benniks <tim@timbenniks.nl>

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
        var id = self.element.attr("id");
        self.element.addClass("widget-active");
        self.item = self.element.find("div.widget-content");

        self.header = self.element.find("h3:first");

        self.icon = $("<i>").appendTo(self.header);
        
	self.options.minifiedOnInit ? self.hide(true) : self.show(true);

        self.header.click(function() 
                          { 
                              self.toggle(); 
                              if (id) z_event("adminwidget_toggle", {id: id, showing: self.showing});
                          });
    },

    toggle: function() {
        var self = this;
        self.setVisible(!self.showing);
    },

    setVisible: function(v, skipAnim) {
        var self = this;
        v ? self.show(skipAnim) : self.hide(skipAnim);
    },
    
    hide: function(skipAnim) {
        var self = this;
        if (skipAnim) 
            self.item.hide();
        else
            self.item.slideUp(200);
        self.icon.attr("class", "pull-right icon-plus");
        self.showing = false;
    },

    show: function(skipAnim) {
        var self = this;
        if (skipAnim) 
            self.item.show();
        else
            self.item.slideDown(200);
        self.icon.attr("class", "pull-right icon-minus");
        self.showing = true;
    }
});

$.z.adminwidget.defaults = {
    minifiedOnInit: false
};