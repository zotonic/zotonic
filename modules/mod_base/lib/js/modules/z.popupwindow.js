/* z.popupwindow.js
----------------------------------------------------------

@author Cody Lindley
@author Marc Worrell <marc@worrell.nl>

http://code.google.com/p/jquery-swip/

Copyright 2009 Cody Lindley
Copyright 2011 Marc Worrell

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

$.widget("ui.popupwindow", {
	_init: function() {
		var self = this;
		$(this.element).click(function() {
			if (self.options.full) {
				self.options.toolbar = 1;
				self.options.scrollbars = 1;
				self.options.location = 1;
				self.options.menubar = 1;
				self.options.centerScreen = 1;
				self.options.centerBrowser = 0;
				self.options.width = screen.width-100;
				self.options.height = screen.height-100;
			}
			var windowFeatures =    'height=' + self.options.height +
									',width=' + self.options.width +
									',toolbar=' + (self.options.toolbar?'yes':'no') +
									',scrollbars=' + (self.options.scrollbars?'yes':'no') +
									',status=' + (self.options.status?'yes':'no') + 
									',resizable=' + (self.options.resizable?'yes':'no') +
									',location=' + (self.options.location?'yes':'no') +
									',menubar=' + (self.options.menubar?'yes':'no');

			var windowName = self.options.windowName;
			var windowURL = this.href || self.options.windowURL;
			var top, left;

			if (self.options.centerBrowser && !self.options.centerScreen) {
				if ($.browser.msie) {
					top = (window.screenTop - 120) + ((((document.documentElement.clientHeight + 120)/2) - (self.options.height/2)));
					left = window.screenLeft + ((((document.documentElement.clientWidth + 20)/2) - (self.options.width/2)));
				} else {
					top = window.screenY + (((window.outerHeight/2) - (self.options.height/2)));
					left = window.screenX + (((window.outerWidth/2) - (self.options.width/2)));
				}
			} else if (self.options.centerScreen) {
				top = (screen.height - self.options.height)/2;
				left = (screen.width - self.options.width)/2;
			} else {
				top = self.options.top;
				left = self.options.left;
			}
			
			/* Remove non-alphanumeric characters for msie */
			if (windowName) {
				windowName = windowName.replace(/[^a-zA-Z0-9]/g,'_');
			}
			var w = window.open(windowURL, windowName, windowFeatures+',left='+Math.ceil(left)+',top='+Math.ceil(top));
			setTimeout(function() {
						if (w.innerWidth != undefined && w.innerWidth > 0) {
							w.resizeBy(self.options.width - w.innerWidth, self.options.height - w.innerHeight);
						}
					}, 500);
			w.focus();
			return false;
		});
	}
});

$.ui.popupwindow.defaults = {
	full:0, // set the height/width to the current window, show scrollbars etc.
	centerBrowser:1, // center window over browser window? {1 (YES) or 0 (NO)}. overrides top and left
	centerScreen:0, // center window over entire screen? {1 (YES) or 0 (NO)}. overrides top and left
	height:500, // sets the height in pixels of the window.
	left:0, // left position when the window appears.
	location:0, // determines whether the address bar is displayed {1 (YES) or 0 (NO)}.
	menubar:0, // determines whether the menu bar is displayed {1 (YES) or 0 (NO)}.
	resizable:0, // whether the window can be resized {1 (YES) or 0 (NO)}. Can also be overloaded using resizable.
	scrollbars:0, // determines whether scrollbars appear on the window {1 (YES) or 0 (NO)}.
	status:0, // whether a status line appears at the bottom of the window {1 (YES) or 0 (NO)}.
	width:500, // sets the width in pixels of the window.
	windowName:null, // name of window
	windowURL:null, // url used for the popup
	top:0, // top position when the window appears.
	toolbar:0 // determines whether a toolbar (includes the forward and back buttons) is displayed {1 (YES) or 0 (NO)}.
};

