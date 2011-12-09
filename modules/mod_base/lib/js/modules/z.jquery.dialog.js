/* jquery dialog js
----------------------------------------------------------

@package:	Zotonic 2011	
@Author: 	Maas-Maarten Zeeman <mmzeeman@xs4all.nl>

Copyright 2011 Maas-Maarten Zeeman

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

;(function( $ )
{
	var $d = null;
	$.extend({
		dialogAdd: function(options)
		{	
			$.dialogRemove();

			var settings = { show: 'fade', hide: 'fade', width: '450px' };

			$.extend(settings, options, { autoOpen: false, 
				close: function(event, ui) { $.dialogRemove(); }
			});
				
			$d = $('<div></div>').hide().html(options.text).dialog(settings);
			$d.dialog('open');            
		},

		dialogClose: function() 
		{
                        if(!$d) return;
			$d.dialog('close');
		},
		
		dialogRemove: function(obj)
		{
                        if(!$d) return;
                        z_tinymce_remove($d);
                        $d.hide();
                        if($d.dialog) $d.dialog('destroy');
                        $d.remove();
                        $d = null;
		}
	});

	// Workaround.. previously this redefined ui.dialog, which was used by widget_manager to show a dialog. 
	$.widget("ui.show_dialog",
        {
                _init: function()
                {
                        var title       = this.options.title;
                        var text        = this.options.text;
                        var width       = this.options.width;

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

        $.ui.show_dialog.defaults = { title: 'Title', text: 'tekst', width: '450px' }
})(jQuery);
