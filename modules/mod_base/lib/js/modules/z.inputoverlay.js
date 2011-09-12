/* inputoverlay js
----------------------------------------------------------

@package:	Zotonic 2010	
@Author:	Marc Worrell <marc@worrell.nl>

Copyright 2010 Marc Worrell

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

/*
This widget overlays a label field with an input field.	 When the input
is empty the label is visible.	When there is content then the label is hidden.

HTML:

<p class="do_inputoverlay">
	<span>Username</span>
	<input type="text" id="username" name="username" value="" />
</p>

CSS:

p.do_inputoverlay {
	margin: 0px;
	padding: 0px;
	position: relative;
	height: 40px;
	font-size: 18px;
}

p.do_inputoverlay input {
	position: absolute;
	left: 0px;
	background: none;
	font-size: 18px;
}

p.do_inputoverlay span {
	position: absolute;
	left: 8px;
	top: 5px;
	color: #aaa;
}

p.do_inputoverlay span.focus {
	color: #d8d8d8;
}

p.do_inputoverlay span.hidden {
	display: none;
}

*/

$.widget("ui.inputoverlay", 
{
	_init: function() 
	{
		var self = this;
		var obj	 = this.element;
		var input = $('input', obj);
		var span = $('span', obj);
		
		if (!input.length) {
			input = $('textarea', obj);
		}
		if ($(input).val() != "") {
			$(span).addClass('hidden');
		}

		var func = function(focus) {
			if ($(input).val() == "") {
				if (focus) {
					$(span).removeClass('hidden').addClass('focus');
				} else {
					$(span).removeClass('hidden').removeClass('focus');
				}
			} else {
				$(span).removeClass('focus').addClass('hidden');
			}
		};
		
		input.change(function() {
			func(true);
		}).focus(function() {
			func(true);
		}).blur(function() {
			func(false);
		}).keydown(function() {
			setTimeout(function(){func(true);},10);
		}).keyup(function() {
			func(true);
		});
		
		input.closest("form").bind("reset", function() {
			setTimeout(function(){func(true);},10);
		});
		
		span.click(function() {
			input.focus();
		});
		
		if (input.attr('autocomplete') == 'on') {
			setInterval(function() {
				if ($(input).val() == "") {
					$(span).removeClass('hidden');
				} else {
					$(span).addClass('hidden');
				}
			}, 100);
		}
	}	 
});
