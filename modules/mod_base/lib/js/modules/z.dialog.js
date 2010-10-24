/* dialog js
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

(function(jQuery)
{
	$.extend({
		dialogAdd: function(options)
		{	
			if(!$('.dialog').length)
			{
				// declare varaibles
				var options, dialogWrapper, dialogTop, dialogTitle, dialogTLC, dialogTRC, dialogClose, dialogContent, dialogInnerContent, dialogRightContent, dialogBottom, dialogBLC, dialogBRC, leftPos, topPos;
				var width;
				
				options = $.extend({}, $.ui.dialog.defaults, options);
				
				if (typeof(options.width) == "number")
					width = options.width + "px";
				else
					width = options.width;
				
				dialogTitle			= $('<h5></h5>').addClass('dialog-title').text(options.title);
				dialogTLC			= $('<span></span>').addClass('dialog-top-left');
				dialogTRC			= $('<span></span>').addClass('dialog-top-right');
				dialogClose			= $('<span></span>').addClass('dialog-close').click(function(){ $.dialogRemove(dialogWrapper); });
				dialogInnerContent	= $('<div></div>').addClass('dialog-inner-content').html(options.text);
				dialogRightContent	= $('<span></span>').addClass('dialog-content-right');
				dialogBLC			= $('<span></span>').addClass('dialog-bottom-left');
				dialogBRC			= $('<span></span>').addClass('dialog-bottom-right');
				leftPos				= Math.floor((parseInt($(window).width()) / 2) - (parseInt(width) / 2));
				topPos				= $(window).scrollTop() + 100;
			
				dialogTop			= $('<div></div>').addClass('dialog-top').append(dialogTitle, dialogTLC, dialogTRC, dialogClose);
				dialogContent		= $('<div></div>').addClass('dialog-content clearfix').append(dialogInnerContent, dialogRightContent);
				dialogBottom		= $('<div></div>').addClass('dialog-bottom').append(dialogBLC, dialogBRC);
			
				dialogWrapper		= $('<div></div>')
										.addClass('dialog')
										.append(dialogTop, dialogContent, dialogBottom)
										.fadeIn(300)
										.css({left: leftPos, top: topPos, width: width})
										.draggable({addClasses: false, handle: dialogTop, opacity: 0.90, zIndex: 2700, iframeFix: true, scroll: true});

				$(document).keypress(function(e)
				{
					if($.browser.msie) 	{ var key = e.which }
					else 				{ key = e.keyCode } 
					
					if(key == $.ui.keyCode.ESCAPE)
					{
						dialogClose.click();
					}
				});
				
				$('body').append(dialogWrapper);
				if (typeof($.widgetManager) != 'undefined') {
					$('.dialog').widgetManager();
				}
				$('.dialog input').eq(0).focus();
			}
		},
		
		dialogRemove: function(obj)
		{
			obj = obj || $('.dialog');
			
			obj.draggable('destroy').resizable('destroy').fadeOut(300, function()
			{
				$(this).remove();
			});
		}
	});
	
	$.widget("ui.dialog", 
	{
		_init: function() 
		{
			var title	= this.options.title;
			var text	= this.options.text;
			var width	= this.options.width;
			
			this.element.click(function()
			{
				$.dialogAdd(
				{
					title: title,
					text:  text,
					width: width
				})
			})
		}
	});
	
	$.ui.dialog.defaults = {
		title: 'Title',
		text: 'tekst',
		width: '450px'
	}
})(jQuery);