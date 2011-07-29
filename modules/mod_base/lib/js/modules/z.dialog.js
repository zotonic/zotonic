/* dialog js
----------------------------------------------------------

@package:	Zotonic 2009	
@Author:	Tim Benniks <tim@timbenniks.nl>

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

(function($)
{
	$.extend({
		dialogAdd: function(options)
		{	
			$('.dialog').remove(); 

			if(!$('.dialog').length)
			{
				// declare varaibles
				var options, dialogWrapper, dialogTop, dialogTitle, dialogTLC, dialogTRC, dialogClose, dialogContent, dialogInnerContent, dialogRightContent, dialogBottom, dialogBLC, dialogBRC, leftPos, topPos;
				var width;
				
				options = $.extend({}, $.ui.dialog.defaults, options);
				
				var dialogClass = 'dialog'
				if (typeof(options.addclass) == "string")
					dialogClass = 'dialog '+options.addclass
					
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
				
				var scrollTop = $(window).scrollTop();
				topPos				= scrollTop + 100;
			
				dialogTop			= $('<div></div>').addClass('dialog-top').append(dialogTitle, dialogTLC, dialogTRC, dialogClose);
				dialogContent		= $('<div></div>').addClass('dialog-content clearfix').append(dialogInnerContent, dialogRightContent);
				dialogBottom		= $('<div></div>').addClass('dialog-bottom').append(dialogBLC, dialogBRC);
			
				dialogWrapper		= $('<div></div>')
										.addClass(dialogClass)
										.append(dialogTop, dialogContent, dialogBottom)
										.fadeIn(300)
										.css({left: leftPos, top: topPos, width: width})
										.draggable({addClasses: false, handle: dialogTop, opacity: 0.90, zIndex: 2700, iframeFix: true, scroll: true});

				$(document).keypress(function(e)
				{
					if($.browser.msie)	{ var key = e.which }
					else				{ key = e.keyCode } 
					
					if(key == $.ui.keyCode.ESCAPE)
					{
						dialogClose.click();
					}
				});
				
				$('body').append(dialogWrapper);

				/* Make sure that the dialog is within the viewport */
				var dialogHeight = dialogWrapper.height();
				var windowHeight = $(window).height();
				if (100 + dialogHeight > windowHeight) {
					var newTop = scrollTop + windowHeight - dialogHeight - 20;
					
					$(dialogWrapper).css({top: newTop > scrollTop ? newTop : scrollTop});
				}

				$('input[type=text]', dialogWrapper).focus();
				if (typeof($.widgetManager) != 'undefined') {
					dialogWrapper.widgetManager();
				}
				z_tinymce_add(dialogWrapper);
			}
		},

		dialogClose: function()
		{
			$('.dialog-close').click();
		},
		
		dialogRemove: function(obj)
		{
			obj = obj || $('.dialog');
			z_tinymce_remove(obj);
			obj.draggable('destroy').resizable('destroy').fadeOut(300, function()
			{
				$(this).remove();
			});
		}
	});
	
	$.widget("ui.show_dialog", 
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
