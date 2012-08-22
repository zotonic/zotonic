/* tooltip js
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

$.widget("ui.tooltip", 
{
	_init: function() 
	{
		var self = this;
		var obj  = this.element;
		var tip  = undefined;
		
		if(this.title == '')
		{
			obj.unbind(self.options.inevent, self.options.outevent);
			return false;
		}

		obj.bind(self.options.inevent, function(e) 
		{
			this.tip 		= this.title;
			var tip_content = this.tip;
			this.title 		= "";
			
			tip = $('<div></div>')
					.addClass('tooltip')
					.html(tip_content)
					.css({top: e.pageY + self.options.offsetY, left: e.pageX + self.options.offsetX, width: self.options.width, maxWidth: self.options.maxwidth });
			
			$(document.body).append(tip);

			var left = $(this).position().left;
			var top  = $(this).position().top;
							
			tip.css({top: top - tip.height()-10});
			
			if(left + tip.width() > $(window).width())
			{
				tip.css({left: Math.ceil(left) - Math.ceil((obj.width() / 2)) - Math.ceil((tip.width() / 2 ))});
			}
			else
			{
				tip.css({left: Math.ceil(left) + Math.ceil((obj.width() / 2)) - Math.ceil((tip.width() / 2 ))});
			}
			
			tip.stop().animate({opacity: 'show'}, 200);
		});
		
		obj.bind(self.options.outevent, function(e) 
		{
			tip.stop().animate({opacity: 'show'}, 200, function()
			{
				$(this).remove();
			});
			
			self.destroy();
			this.title = this.tip;
		});
	},
	destroy: function()
	{
	    var self = this;
		var obj  = this.element;

		obj.unbind(self.options.inevent, self.options.outevent);
		$(document).unbind('mousemove');
	}
});

$.fn.tooltip.destroy = function()
{
	$('.tooltip').tooltip('destroy');
};

$.ui.tooltip.defaults = {
	offsetY: 	0,
	offsetX: 	0,
	inevent: 	'mouseover',
	outevent: 	'mouseout',
	width: 		'auto',
	maxwidth: 	'330px'
};
