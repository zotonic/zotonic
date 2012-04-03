/* dialog js
 ----------------------------------------------------------

 @package:	Zotonic 2009, 2012
 @Author:	Tim Benniks <tim@timbenniks.nl>

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

(function($)
 {
     $.extend(
         {
             dialogAdd: function(options)
	     {	
		 $('#zmodal').remove(); 

		 options = $.extend({}, $.ui.dialog.defaults, options);
		     		     
		 var width = options.width;

		 var title = $("<div>").addClass("modal-header")
                     .append($("<a>").addClass("close").attr("data-dismiss", "modal").html("&times;"))
                     .append($("<h3>").text(options.title));

                 var body = $("<div>").addClass("modal-body")
                     .html(options.text);
                 
		 var dialogClass = 'modal';
		 if (typeof(options.addclass) == "string")
		     dialogClass += ' ' + options.addclass;

                 var dialog = $("<div>")
                     .attr("id", "zmodal")
                     .attr("style", "width: "+width)
                     .addClass(dialogClass)
                     .append(title)
                     .append(body)
                     .appendTo($("body"));

                 dialog.modal({backdrop: true});

		 if (typeof($.widgetManager) != 'undefined') {
		     dialog.widgetManager();
		 }
		 z_tinymce_add(dialog);

	     },

	     dialogClose: function()
	     {
		 $('#zmodal').modal('hide');
	     },
	     
	     dialogRemove: function(obj)
	     {
		 obj = obj || $('#zmodal');
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
				                 });
			                 });
		  }
	      });
     
     $.ui.dialog.defaults = {
	 title: 'Title',
	 text: 'tekst',
	 width: '700px'
     };
 })(jQuery);
