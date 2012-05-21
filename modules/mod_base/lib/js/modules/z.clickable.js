/* clickable js
----------------------------------------------------------

@package:	Zotonic 2012	
@Author: 	Joost Faber <info@joostfaber.nl>

Copyright 2012 Joost Faber

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

$.widget("ui.clickable", 
{
	_init: function() 
	{
		$(this.element).click(function(e) {
			var target = $(this).find("a").attr("href");
			if($(this).find("a").attr("rel") == "external"){
				window.open(target);
			} else {
				window.location=target;
			}
			return false;
		}).addClass("clickable").hover(
			function () {
				$(this).addClass("hover");	    	
		    }, 
		    function () {		
				$(this).removeClass("hover");	
		  	}
	 	);

		// Make sure that checkboxes still work
		$(":checkbox", this.element).click(function(e) {
			var checkbox = this;
			setTimeout(function() { 
					if ($(checkbox).attr('checked'))
						$(checkbox).attr('checked', false);
					else
						$(checkbox).attr('checked', true);
				 }, 10);
			e.stopPropagation();
			return false; 
		});
	}
});

$.ui.clickable.defaults = {
}
