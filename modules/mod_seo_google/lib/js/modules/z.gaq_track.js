/* z.gaq_track.js
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

$.widget("ui.gaq_track", {
	_init: function() {
		var self = this;
		
		if (self.options.on_click) {
			$(self.element).click(function() {
				self.track("Click");
			});
		}
		if (self.options.on_view) {
			$(self.element).each(function() {
				self.track("View");
			});
		}
	},
	
	track: function(action) {
		var args = ["_trackEvent"];
		args.push(this.options.category);
		args.push(action);
		if (this.options.label != undefined) {
			args.push(this.options.label)
			if (this.options.value != undefined) {
				args.push(this.options.value)
			}
		}
		if (typeof _gaq == "object") {
			_gaq.push(args);
		}
	}
});

$.ui.gaq_track.defaults = {
	category: 'Ad',
	label: undefined,
	value: undefined,
	on_view: true,
	on_click: true
};
