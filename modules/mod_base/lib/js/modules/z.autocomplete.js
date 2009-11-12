/* autocomplete js
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

$.widget("ui.autocomplete", 
{
	_init: function() 
	{
		var self			= this;
		var obj 			= this.element;	
		var inputWidth 		= obj.width() + parseInt(obj.css('padding-left')) + parseInt(obj.css('padding-right'));
		var ulTop 			= obj.position().top + obj.height() + parseInt(obj.css('padding-top')) + parseInt(obj.css('padding-bottom'));
		var suggestions 	= $('<ul></ul>').addClass('suggestions-list').css({width: inputWidth, position: 'absolute', top: ulTop, left: obj.position().left}).hide().appendTo(document.body);
		var input_updater 	= false;
	
		obj.bind('keyup', function()
		{
			if(input_updater)
			{
				clearTimeout(input_updater);
				input_updater = false;
			}
			
			input_updater = setTimeout(function()
			{
				if(obj.val().length >= self.options.afterChars)
				{
					obj.addClass('loading');
				
					$.post(self.options.controller, 
					{
						input: 			obj.val(),
						obj:   			obj,
						suggestions: 	suggestions
					});
				}
			}, 400);
		});
	},
	
	kill: function() 
	{
		this.destroy();
	}
});

$.ui.autocomplete.defaults = {
	controller: '/postback',
	afterChars: 3
}

var z_autoCompleteAfterPostback = function(obj, suggestions, data)
{
	if(data)
	{
		obj.removeClass('loading');
		suggestions.animate({height: 'show', opacity: 'show'}, 200).html(data);

		$('.suggestions-result').hover(function() 
		{
			$(this).addClass('hovering')
		},
		function()
		{
			$(this).removeClass('hovering')
		})
		.click(function()
		{
			obj.val($(this).html());
			suggestions.animate({height: 'hide', opacity: 'hide'}, 200);
			$.ui.autocomplete.kill();
		});
	}
}