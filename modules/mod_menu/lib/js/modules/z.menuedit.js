/* z.menuedit.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>

Copyright 2011 Marc Worrell

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

$.widget("ui.menuedit", {
    _init: function() {
        var self = this;
        var suppress_update = false;

        z_ensure_id(self.element);
        self.options.listType = self.element[0].nodeName.toLowerCase();

        self.options.update = function() {
            if (suppress_update) {
                suppress_update = false;
            } else if (self.options.z_notify != undefined) {
                var h = $(this).nestedSortable('toHierarchy', { expression: /()(.*)/ });
                var hs = self.flatten(h);
                var args = jQuery.extend(true, {}, self.options.z_args);
                args.tree = hs;
                args.action = "sort";
                args.z_trigger_id = $(self.element).attr('id');
                args.z_delegate = self.options.z_delegate;
                z_notify(self.options.z_notify, args);
            }
        };
        self.options.receive = function(event, ui) {
            var elt = ui.item[0];
            var elt_id = $(elt).attr('id');
            var target_elt = $('.drag_group_dragdrop', $(this));
            var new_id = z_unique_id()+"-"+elt_id;

            $(target_elt)
                .attr('id', new_id)
                .attr('class', '');

            var h = $(this).nestedSortable('toHierarchy', { expression: /()(.*)/ });
            var hs = self.flatten(h);

            if (self.options.z_notify != undefined) {
                var args = jQuery.extend(true, {}, self.options.z_args);
                args.action = "receive";
                args.tree = hs;
                args.dragged_id = elt_id;
                args.dragged_data = $(elt).data('nestedsortable-receive');
                args.z_target_id = new_id;
                args.z_trigger_id = $(self.element).attr('id');
                args.z_delegate = self.options.z_delegate;
                z_notify(self.options.z_notify, args);
                suppress_update = true;
            }
        };
        $(self.element).bind('sortupdate', self.options.update);
        $(self.element).nestedSortable(self.options);
    },
    
    flatten: function(h) {
        var s = "[";
        var n = h.length;
        for (var i=0; i<n; i++) {
            if (i > 0) s +=",";
            s += h[i].id;
            if (h[i].children != undefined) {
                s += this.flatten(h[i].children);
            }
        }
        return s+"]";
    }
});

$.ui.menuedit.defaults = {
	disableNesting: "ui-nestedSortable-no-nesting",
	forcePlaceholderSize: true,
	handle: "div",
	helper: "clone",
	items: "li.menu-item",
	maxLevels: 0,
	opacity: .6,
	placeholder: "placeholder",
	revert: 250,
	tabSize: 20,
	tolerance: "pointer",
	toleranceElement: "> div",

	z_notify: "menuedit",
	z_delegate: "mod_menu",
	z_args: {}
};
