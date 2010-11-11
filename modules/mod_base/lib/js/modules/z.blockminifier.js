/* blockminifier js
----------------------------------------------------------

@package:	Zotonic 2009	
@Author: 	Tim Benniks <tim@timbenniks.nl>

Copyright 2009 Tim Benniks

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

$.widget("ui.blockminifier", 
{
	_init: function() 
	{
		var self = this;
        self.item = self.element.next(self.options.itemToMinify);

		if(self.options.minifiedOnInit)
		{
			self.element.addClass('above-item-all-corners');
			self.item.hide();
			
			self.element.toggle(function()
			{
				self.element.removeClass('above-item-all-corners');
				self.item.slideDown(200);
				$("#map_canvas", self.item).each(function() {
					google.maps.event.trigger(googleMapsControl.getMap(), 'resize');
				});
			},
			function()
			{
				self.item.slideUp(200, function()
				{
					self.element.addClass('above-item-all-corners');
				});
			});
		}
		else
		{
			self.element.toggle(function()
			{
				self.item.slideUp(200, function()
				{
					self.element.addClass('above-item-all-corners');
				});
			},
			function()
			{
				self.element.removeClass('above-item-all-corners');
				self.item.slideDown(200);
				$("#map_canvas", self.item).each(function() {
					google.maps.event.trigger(googleMapsControl.getMap(), 'resize');
				});
			});
		}
	}
});

$.ui.blockminifier.defaults = {
	minifiedOnInit: false,
	itemToMinify: '.item'
}