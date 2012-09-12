/* make_diff js
----------------------------------------------------------

@package:	Zotonic 2012	
@Author: 	Marc Worrell <marcw@worrell.nl>

Copyright 2012 Marc Worrell

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

$.widget("ui.make_diff", 
{
	_init: function() 
	{
		var k = $(this.element).children('th').html();
		var tds = $(this.element).children('td');
		var a = $(tds.get(0)).html();
		var b = $(tds.get(1)).html();
		var diff = WDiffShortenOutput(WDiffString(b,a));

		// diff = diff.replace(/\n\n--\n\n/g, "<hr class='make-diff-x' />")
		// 		   .replace(/\n\n/g, "<br class='make-diff-x' />")
		// 		   ;

		$(this.element).html("<th>"+k+"</th><td class='make-diff-top' colspan='2'>"+diff+"</td>");

		// // Remove top elements that are not changed.
		// $(this.element).children('td').contents().filter(function() {
		// 		  return this.nodeType == 3;
		// 	})
	 //  		.wrap('<span class="make-diff-x"></span>');
		// $(this.element).children('td').children().addClass("make-diff-delete-me");

		// $('ins,del', $(this.element)).each(function() {
		// 	var $elt = $(this);
		// 	while (!$elt.parent().hasClass('make-diff-top')) {
		// 		$elt = $elt.parent();
		// 	}
		// 	$elt.removeClass('make-diff-delete-me');

		// 	var $x = $elt;
		// 	do {
		// 		$x = $x.next();
		// 		$x.removeClass('make-diff-delete-me');
		// 	} while ($x.is('br') || $x.is('hr') || $x.hasClass('make-diff-x'));

		// 	$x = $elt;
		// 	do {
		// 	 	$x = $x.prev();
		// 	 	$x.removeClass('make-diff-delete-me');
		// 	} while ($x.is('br') || $x.is('hr') || $x.hasClass('make-diff-x'));
		// });
		// $('.make-diff-delete-me').remove();
	}

});

$.ui.make_diff.defaults = {
}

window.wDiffOmittedChars = " <span class='diff-omitted'>[…]</span> ";
window.wDiffOmittedLines = "<hr class='diff-omitted' />";
window.wDiffNoChange = "<hr class='diff-nochange' />";


