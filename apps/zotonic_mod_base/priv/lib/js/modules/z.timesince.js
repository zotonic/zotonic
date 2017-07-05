/* z.timesince.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>
@author Erik Hanson

Incorporates code by Erik Hanson:
http://www.eahanson.com/2008/12/04/relative-dates-in-javascript/

Simple relative date.

Shows a string like "4 days ago". Prefers to return values >= 2. For example, it would
show "26 hours ago" instead of "1 day ago", but would show "2 days ago" instead of
"49 hours ago".

Copyright 2011 Marc Worrell
Copyright (c) 2008 Erik Hanson http://www.eahanson.com/

Licensed under the MIT License http://www.opensource.org/licenses/mit-license.php

---------------------------------------------------------- */

$.widget("ui.timesince", {
	_init: function() {
		var self = this;
		var olderDate = this.options.time;

		if (typeof olderDate == "string") 
			olderDate = new Date(olderDate);
		else if (typeof olderDate == "number")
			olderDate = new Date(olderDate * 1000);

		self.element.html(self.timesince(olderDate));
		setInterval(function() {
						self.element.html(self.timesince(olderDate));
					}, 10000);
	},
		
	timesince: function(olderDate) {
		var newerDate = new Date();
		var milliseconds = newerDate - olderDate;
		var conversions = [
			["years", 31518720000],
			["months", 2626560000 /* assumes there are 30.4 days in a month */],
			["days", 86400000],
			["hours", 3600000],
			["minutes", 60000],
			["seconds", 1000]
		];

		for (var i = 0; i < conversions.length; i++) {
			var result = Math.floor(milliseconds / conversions[i][1]);
			if (result >= 10 || (result >= 2 && conversions[i][1] > 1000)) {
				return result + " " + z_translate(conversions[i][0] + " ago");
			}
		}
		return z_translate("moments ago");
	}
});

$.ui.timesince.defaults = {
	time: 0		/* Unix timestamp, seconds since Jan 1st, 1970 */
};

