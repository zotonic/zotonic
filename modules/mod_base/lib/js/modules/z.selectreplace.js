/* select replace js
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
	//create cross-browser indexOf
	Array.prototype.indexOf = function (obj, start) 
	{
		for(var i = (start || 0); i < this.length; i++) 
		{
			if(this[i] == obj) 
			{
				return i;
			}
		}
	}

	$.fn.selectReplace = function(options) 
	{
		return this.each(function()
		{
			var defaults = 
			{
				defaultText: 'Please select',
				animationSpeed: 0,
				ddMaxHeight: ''
			};

			var opts 				= $.extend(defaults, options),
				$input 				= $(this),
				$containerDivText 	= $('<div></div>').addClass('select-replace-selected-text'),
				$containerDiv 		= $('<div></div>').addClass('select-replace-new-list-selected').attr('tabindex', 0),
				$newUl 				= $('<ul></ul>').addClass('select-replace-new-list'),
				itemIndex 			= -1,
				currentIndex 		= -1,
				keys 				= [],
				prevKey 			= false,
				newListItems 		= '',
				prevented 			= false;
				
			$containerDiv.insertAfter($input);
			$containerDivText.prependTo($containerDiv);
			$newUl.appendTo($containerDiv);
			$input.hide();
		
			if($input.children('optgroup').length == 0)
			{
				$input.children().each(function(i)
				{
					var option = $(this).text();
					
					keys.push(option.charAt(0).toLowerCase());
					
					if($(this).attr('selected') == true)
					{
						opts.defaultText = option;
						currentIndex 	 = i;
					}
				
					newListItems += '<li>'+option+'</li>';
				});
				
				$newUl.html(newListItems);
				newListItems 	= '';
				var $newLi 		= $newUl.children();
								
			} 
			else 
			{
				$input.children('optgroup').each(function(i)
				{
					var optionTitle = $(this).attr('label'),
						$optGroup 	= $('<li class="newListOptionTitle">'+optionTitle+'</li>');
						
					$optGroup.appendTo($newUl);

					var $optGroupList = $('<ul></ul>');

					$optGroupList.appendTo($optGroup);

					$(this).children().each(function()
					{
						++itemIndex;
						var option = $(this).text();
					
						keys.push(option.charAt(0).toLowerCase());
						
						if($(this).attr('selected') == true)
						{
							opts.defaultText = option;
							currentIndex = itemIndex;
						}
						
						newListItems += '<li>'+option+'</li>';
					})

					$optGroupList.html(newListItems);
					newListItems = '';
				});

				var $newLi = $newUl.find('ul li');			
			}
			
			var newUlHeight 	= $newUl.height(),
				containerHeight = $containerDiv.height(),
				newLiLength 	= $newLi.length;
		
			if (currentIndex != -1)
			{
				navigateList(currentIndex, true);
			} 
			else 
			{
				$containerDivText.text(opts.defaultText);
			}

			function newUlPos()
			{
				var containerPosY 	= $containerDiv.offset().top,
					docHeight 		= jQuery(window).height(),
					scrollTop 		= jQuery(window).scrollTop();

					if(newUlHeight > parseInt(opts.ddMaxHeight)) 
					{
						newUlHeight = parseInt(opts.ddMaxHeight);
					}	

				containerPosY = containerPosY-scrollTop;

				if(containerPosY+newUlHeight >= docHeight)
				{
					$newUl.css({top: '-'+newUlHeight+'px', height: newUlHeight});
					$input.onTop = true;
				} 
				else 
				{
					$newUl.css({top: containerHeight+'px', height: newUlHeight});
					$input.onTop = false;
				}
			}

			newUlPos();

			$(window).resize(function()
			{
				newUlPos();
			});
			
			$(window).scroll(function()
			{
				newUlPos();
			});

			function positionFix()
			{
				$containerDiv.css('position','relative');
			}

			function positionHideFix()
			{
				$containerDiv.css('position','static');
			}
			
			$containerDivText.click(function()
			{
				if ($newUl.is(':visible'))
				{
					$newUl.hide();
					positionHideFix()
					return false;
				}

				$containerDiv.focus();

				$newUl.slideDown(opts.animationSpeed);
				positionFix();
				$newUl.scrollTop($input.liOffsetTop);
			});
			
			$newLi.hover(function(e)
			{
				var $hoveredLi = $(e.target);
				$hoveredLi.addClass('select-replace-hover');
			},
			function(e) 
			{
				var $hoveredLi = $(e.target);
				$hoveredLi.removeClass('select-replace-hover');
			});

			$newLi.click(function(e)
			{
				var $clickedLi 	= $(e.target);
				currentIndex 	= $newLi.index($clickedLi);
				prevented 		= true;
				
				navigateList(currentIndex);
				$newUl.hide();
				$containerDiv.css('position','static');
			});

			function navigateList(currentIndex, init)
			{
				var containerOffsetTop  = $containerDiv.offset().top,
					liOffsetTop 		= $newLi.eq(currentIndex).offset().top,
					ulScrollTop 		= $newUl.scrollTop();

				if($input.onTop == true)
				{
					$input.liOffsetTop = (((liOffsetTop-containerOffsetTop)-containerHeight)+ulScrollTop)+parseInt(opts.ddMaxHeight);
				}
				else 
				{
					$input.liOffsetTop = ((liOffsetTop-containerOffsetTop)-containerHeight)+ulScrollTop;
				}
				
				$newUl.scrollTop($input.liOffsetTop);
				
				$newLi.removeClass('select-replace-highlite').eq(currentIndex).addClass('select-replace-highlite');

				var text = $newLi.eq(currentIndex).text();
	
				if(init == true)
				{
					$input.val(text);
					$containerDivText.text(text);
					return false;
				}
				
				$input.val(text).change();
				$containerDivText.text(text);

			};

			$input.change(function(event)
			{
				$targetInput = $(event.target);
				
				if(prevented == true)
				{
					prevented = false;
					return false;
				}
				
				$currentOpt  = $targetInput.find(':selected');
				currentIndex = $targetInput.find('option').index($currentOpt);

				navigateList(currentIndex, true);
			});
			
			function keyPress(element) 
			{
				element.onkeydown = function(e)
				{
					if (e == null) 
					{
						var keycode = event.keyCode;
					} 
					else 
					{
						var keycode = e.which;
					}
					
					prevented = true;

					switch(keycode)
					{
					case 40: //down
					case 39: //right
						incrementList();
						return false;
						break;
					case 38: //up
					case 37: //left
						decrementList();
						return false;
						break;
					case 33: //page up
					case 36: //home
						gotoFirst();
						return false;
						break;
					case 34: //page down
					case 35: //end
						gotoLast();
						return false;
						break;
					case 13:
					case 27:
						$newUl.hide();
						positionHideFix();
						return false;
						break;
					}

					keyPressed 			= String.fromCharCode(keycode).toLowerCase();
					var currentKeyIndex = keys.indexOf(keyPressed);
					
					if(typeof currentKeyIndex != 'undefined') 
					{
						++currentIndex;
						currentIndex = keys.indexOf(keyPressed, currentIndex); //search array from current index
						
						if(currentIndex == -1 || currentIndex == null || prevKey != keyPressed) currentIndex = keys.indexOf(keyPressed); //if no entry was found or new key pressed search from start of array
						navigateList(currentIndex);
						
						prevKey = keyPressed;
						return false;
					}
				}
			}

			function incrementList()
			{
				if(currentIndex < (newLiLength-1)) 
				{
					++currentIndex;
					navigateList(currentIndex);
				}
			}

			function decrementList()
			{
				if(currentIndex > 0) 
				{
					--currentIndex;
					navigateList(currentIndex);
				}
			}

			function gotoFirst()
			{
				currentIndex = 0;
				navigateList(currentIndex);
			}
			
			function gotoLast()
			{
				currentIndex = newLiLength-1;
				navigateList(currentIndex);
			}

			$containerDiv.click(function()
			{
				keyPress(this);
			});

			$containerDiv.focus(function()
			{
				$(this).addClass('select-replace-focus');
				keyPress(this);
			});
			
			$containerDiv.blur(function()
			{
			   $(this).removeClass('select-replace-focus');
			   $newUl.hide();
			   positionHideFix();
			});

			$newUl.css('left','0').hide();
		});
	};
})(jQuery);