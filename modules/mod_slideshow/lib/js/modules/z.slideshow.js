/* Slideshow popup
   collects all do_slideshow ids and starts slideshow
----------------------------------------------------------

@package:	Zotonic	
@Author: 	Marc Worrell <marc@worrell.nl>

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

$.widget("ui.slideshow", 
{
	_init: function() 
	{
		var self = this;
		var obj  = this.element;
		
		$(this.element).click(function() {
			// Collect all ids
			var data_name = 'widget-slideshow';
			var args = [ {name:"start_id", value:self.options.id} ];
			
			$(".do_slideshow").each(function() {
				var data = $(this).data(data_name);
				if (data && data.id) {
					args.push({name: "id", value: data.id});
				}
			});
			
			if (args.length) {
				z_notify("slideshow", args);
			}
		});
	}
});

$.ui.slideshow.defaults = {
	id: false
}
