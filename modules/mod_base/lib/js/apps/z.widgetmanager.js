/* Admin widgetManager class
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

;(function($)
{
	$.extend(
	{
		widgetManager: function(context)
		{
			context 	= context || document.body;
			var stack 	= [context];

			while(stack.length > 0)
			{
				var objectClass, objectOptions, functionName, defaults, element = stack.pop();
				
				if(element.className && (objectClass = new RegExp("do_([\\w_]+)", "g").exec(element.className)))
				{
					functionName = objectClass[1];

					if('dialog' == functionName) functionName = 'show_dialog'; // work around to prevent ui.dialog redefinition
					
					if(typeof $(element)[functionName] == "function")
					{
						if($.ui && $.ui[functionName] && $.ui[functionName].defaults)
						{
							defaults = $.ui[functionName].defaults;
						}
						else
						{
							defaults = {}
						}
						
						objectOptions = $.extend({}, defaults, $(element).metadata());
						$(element)[functionName](objectOptions);
					}
				}

				if(element.childNodes) 
				{
				    for(var i = 0; i< element.childNodes.length; i++)
					{
						if(element.childNodes[i].nodeType != 3)
						{
							stack.unshift(element.childNodes[i]);
						}
					}
				}
			}
		},
		
		misc: 
		{
			log: function(obj)
			{
				if(window.console) 
				{
					console.log(obj);
	
					if($.noticeAdd)
					{
						$.noticeAdd({
							text: 'Logging, check firebug: '+obj, 
							type: 'notice', 
							stay: 0
						});
					}
				} 
				else 
				{
					if($.noticeAdd)
					{
						$.noticeAdd({
							text: 'logged: '+obj, 
							type: 'notice', 
							stay: 0
						});
					}
					else
					{
						alert(obj.toSource());
					}
				}
			},
			
			warn: function(text, obj)
			{
				obj = obj || '';
				
				if(window.console) 
				{
					console.warn(text, obj);
				}
				
				if($.noticeAdd)
				{
					$.noticeAdd({
						text: text, 
						type: 'notice', 
						stay: 1
					});
				}
			},
			
			error: function(text, obj)
			{
				obj = obj || '';

				if(window.console) 
				{
					console.error(text, obj);
				}
				
				if($.noticeAdd)
				{
					$.noticeAdd({
						text: text, 
						type: 'error', 
						stay: 1
					});
				}
			}
		},
		
		metadata: 
		{
			defaults: {
				type: 'class',
				name: 'metadata',
				cre: /({.*})/,
				single: 'metadata'
			},
			
			setType: function(type, name)
			{
				this.defaults.type = type;
				this.defaults.name = name;
			},
			
			get: function(elem, opts)
			{
				var settings = $.extend({}, this.defaults, opts);
				
				// check for empty string in single property
				if(!settings.single.length)
				{
					settings.single = 'metadata';
				}
				
				var data = $.data(elem, settings.single);
				
				// returned cached data if it already exists
				if(data)
				{
					return data;
				}
				
				data = "{}";
				
				if(settings.type == "class")
				{
					var m = settings.cre.exec(elem.className);
					if(m)
					{
						data = m[1];
					}
				} 
				else if(settings.type == "elem")
				{
					if(!elem.getElementsByTagName)
					{
						return;
					}
					
					var e = elem.getElementsByTagName(settings.name);
					
					if(e.length)
					{
						data = $.trim(e[0].innerHTML);
					}
				} 
				else if(elem.getAttribute != undefined)
				{
					var attr = elem.getAttribute(settings.name);
					if(attr)
					{
						data = attr;
					}
				}
				
				if(data.indexOf( '{' ) < 0)
				data = "{" + data + "}";
				
				data = eval("(" + data + ")");
				
				$.data(elem, settings.single, data);
				return data;
			}
		}
	});
	
	$.fn.metadata = function(opts)
	{
		return $.metadata.get( this[0], opts );
	};
    
	$.fn.widgetManager = function(opts)
	{
	    $.widgetManager(this[0]);
	};

})(jQuery);
