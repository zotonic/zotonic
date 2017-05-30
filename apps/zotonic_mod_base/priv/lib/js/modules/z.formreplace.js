/* fieldreplace js
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

$.widget("ui.fieldreplace", 
{
	_init: function() 
	{		
		//if(!this.element.is(':checkbox, :radio')) return false;
        if ($(this).data("fieldreplaced"))
        {
            return false;
        }
        $(this).data("fieldreplaced", true);
		var fieldWrapper, replacedField, self = this, obj = this.element;

		fieldWrapper 	= $('<span></span>').addClass('zp-field-wrapper zp-' + obj.get(0).type);
		replacedField 	= $('<span></span>').addClass(obj.is(':checked') ? 'zp-'+ obj.get(0).type +'-checked' : 'zp-'+ obj.get(0).type +'-unchecked').addClass('zp-'+ obj.get(0).type +'-replacement').bind('click', function(e) { obj.click(); });

		obj.wrap(fieldWrapper).after(replacedField).addClass('zp-'+ obj.get(0).type +'-replaced').bind('change', function(e) { replacedField.toggleClass('zp-'+ obj.get(0).type +'-unchecked'); replacedField.toggleClass('zp-'+ obj.get(0).type +'-checked'); });
	}
});