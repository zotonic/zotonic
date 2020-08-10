/* Admin widgetManager class
----------------------------------------------------------

@package:	Zotonic
@Author:	Tim Benniks <tim@timbenniks.nl>
@Author:	Marc Worrell <marc@worrell.nl>

Copyright 2009-2011 Tim Benniks

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
            context		= context || document.body;
            var stack	= [context];

            while (stack.length > 0)
            {
                var defaults, element = stack.pop();
                if (typeof element.className == "string")
                {
                    var objectClass = element.className.match(/do_[a-zA-Z0-9_]+/g);
                    if (objectClass)
                    {
                        var n = objectClass.length;
                        for (var i=0; i<n; i++)
                        {
                            var functionName = objectClass[i].substring(3);
                            var defaultsName = functionName;

                            if ('dialog' == functionName) functionName = 'show_dialog'; // work around to prevent ui.dialog redefinition

                            if (typeof $(element)[functionName] == "function")
                            {
                                if ($.ui && $.ui[functionName] && $.ui[functionName].defaults)
                                {
                                    defaults = $.ui[functionName].defaults;
                                }
                                else
                                {
                                    defaults = {}
                                }
                                $(element)[functionName]( $.extend({}, defaults, $(element).metadata(defaultsName)) );
                            }
                        }
                    }
                }

                if (element.childNodes)
                {
                    for (var i = 0; i< element.childNodes.length; i++)
                    {
                        if (element.childNodes[i].nodeType != 3)
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
                var text = obj.toString();
                if(window.console)
                {
                    console.log(text);

                    if($.noticeAdd)
                    {
                        $.noticeAdd({
                            text: 'Logging, check firebug: '+text,
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
                            text: 'logged: '+text,
                            type: 'notice',
                            stay: 0
                        });
                    }
                    else
                    {
                        alert(text);
                    }
                }
            },

            warn: function(text, obj)
            {
                obj = obj || '';

                if(window.console)
                {
                    console.warn(text, obj.toString());
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
                    console.error(text, obj.toString());
                    if (obj.stack)
                        console.error(obj.stack);
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
        }
    });

    $.fn.metadata = function(functionName)
    {
        var elem = this[0];
        var data_name = 'widget-'+functionName;
        var data = $(elem).data(data_name);
        if(typeof data === "undefined")
        {
            data = elem.getAttribute("data-"+functionName);
            if (data) {
                if (data.substr(0,1) == "{") {
                    try {
                        data = JSON.parse(data);
                    } catch (e) {
                        console.error("Error parsing JSON in widget data attribute:", data);
                        data = {};
                    }
                } else {
                    try {
                        data = eval("({" + data.replace(/[\n\r]/g,' ') + "})");
                    } catch (e) {
                        console.error("Error evaluating widget data attribute:", data);
                        data = {};
                    }
                }
            } else {
                data = {};
            }
            $(elem).data(data_name, data);
        }
        return data;
    };

    $.fn.widgetManager = function()
    {
        this.each(function() { $.widgetManager(this); });
        return this;
    };

    // Make jQuery UI optional
    if (typeof $.ui === 'undefined') {
        $.ui = {
            dialog: {}
        };
    }

    if (typeof $.widget !== 'function')
    {
        $.widget = function( widgetName, base ) {
            let name = widgetName.replace(/^ui\./, '');
            $.ui[name] = {
                defaults: {}
            };
            $.fn[name] = function( options ) {
                let w = {
                    element: this,
                    options: $.extend({}, $.ui.defaults, options)
                }
                if (base._init) {
                    base._init.call(w);
                }
            }
        }
    }

})(jQuery);
