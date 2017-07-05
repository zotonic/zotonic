/* z.smiley.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>
@author Ben Alman

Incorporates code by Ben Alman:
http://benalman.com/projects/jquery-replacetext-plugin/

Copyright 2011 Marc Worrell
Copyright 2009 Ben Alman

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

$.widget("ui.smiley", {
	_init: function() {
		var html = $(this.element).html;
		var smileys = this.options.smileys;

		this.element.each(function(){
			var node = this.firstChild, val, new_val, remove = [];
			if (node) {
				do {
					if ( node.nodeType === 3 ) {
						val = node.nodeValue;
						new_val = val;
						for (var i=0; i<smileys.length; i++) {
							var img = '<img src="'+smileys[i].u+'" alt="'+smileys[i].d+'"/>';
							new_val = new_val.replace(smileys[i].k, img);
						};
						if ( new_val !== val ) {
							$(node).before( new_val );
							remove.push( node );
						}
					}
				} while ( node = node.nextSibling );
			}
			remove.length && $(remove).remove();
		});
	}
});

$.ui.smiley.defaults = {
	smileys: [
		{ k:'(ff)', d:"big hug", u:"/lib/images/smileys/firefox.gif" },
		{ k:'>:D<', d:"big hug", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/6.gif" },
		{ k:'#:-S', d:"whew!", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/18.gif" },
		{ k:'O:-)', d:"angel", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/25.gif" },
		{ k:'<:-P', d:"party", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/36.gif" },
		{ k:':-SS', d:"nail biting", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/42.gif" },
		{ k:'<):)', d:"cowboy", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/48.gif" },
		{ k:':-bd', d:"thumbs up", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/113.gif" },
		{ k:'^#(^', d:"it wasn't me", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/114.gif" },
		{ k:':ar!', d:"pirate", u:"http://l.yimg.com/a/i/us/msg/emoticons/pirate_2.gif" },
		{ k:':-??', d:"I don't know", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/106.gif" },
		{ k:'3:-O', d:"cow", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/50.gif" },
		{ k:':(|)', d:"monkey", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/51.gif" },
		{ k:'@};-', d:"rose", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/53.gif" },
		{ k:'**==', d:"flag", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/55.gif" },
		{ k:'(~~)', d:"pumpkin", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/56.gif" },
		{ k:'*-:)', d:"idea", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/58.gif" },
		{ k:'[-O<', d:"praying", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/63.gif" },
		{ k:':)>-', d:"peace sign", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/67.gif" },
		{ k:'\\:D/', d:"dancing", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/69.gif" },
		{ k:'^:)^', d:"not worthy", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/77.gif" },
		{ k:'[..]', d:"transformer", u:"http://l.yimg.com/a/i/us/msg/emoticons/transformer.gif" },
		{ k:';;)', d:"batting eyelashes", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/5.gif" },
		{ k:':-/', d:"confused", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/7.gif" },
		{ k:':">', d:"blushing", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/9.gif" },
		{ k:':-*', d:"kiss", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/11.gif" },
		{ k:'=((', d:"broken heart", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/12.gif" },
		{ k:':-O', d:"surprise", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/13.gif" },
		{ k:'B-)', d:"cool", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/16.gif" },
		{ k:':-S', d:"worried", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/17.gif" },
		{ k:'>:)', d:"devil", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/19.gif" },
		{ k:':((', d:"crying", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/20.gif" },
		{ k:':))', d:"laughing", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/21.gif" },
		{ k:'/:)', d:"raised eyebrows", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/23.gif" },
		{ k:'=))', d:"rolling on the floor", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/24.gif" },
		{ k:':-B', d:"nerd", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/26.gif" },
		{ k:':-c', d:"call me", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/101.gif" },
		{ k:':)]', d:"on the phone", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/100.gif" },
		{ k:'~X(', d:"at wits' end", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/102.gif" },
		{ k:':-h', d:"wave", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/103.gif" },
		{ k:':-t', d:"time out", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/104.gif" },
		{ k:'8->', d:"day dreaming", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/105.gif" },
		{ k:'I-)', d:"sleepy", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/28.gif" },
		{ k:'8-|', d:"rolling eyes", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/29.gif" },
		{ k:'L-)', d:"loser", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/30.gif" },
		{ k:':-&', d:"sick", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/31.gif" },
		{ k:':-$', d:"don't tell anyone", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/32.gif" },
		{ k:'[-(', d:"no talking", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/33.gif" },
		{ k:':O)', d:"clown", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/34.gif" },
		{ k:'8-}', d:"silly", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/35.gif" },
		{ k:'(:|', d:"yawn", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/37.gif" },
		{ k:'=P~', d:"drooling", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/38.gif" },
		{ k:':-?', d:"thinking", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/39.gif" },
		{ k:'#-o', d:"d'oh", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/40.gif" },
		{ k:'=D>', d:"applause", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/41.gif" },
		{ k:'@-)', d:"hypnotized", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/43.gif" },
		{ k:':^o', d:"liar", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/44.gif" },
		{ k:':-w', d:"waiting", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/45.gif" },
		{ k:':-<', d:"sigh", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/46.gif" },
		{ k:'>:P', d:"phbbbbt", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/47.gif" },
		{ k:'X_X', d:"I don't want to see", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/109.gif" },
		{ k:':!!', d:"hurry up!", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/110.gif" },
		{ k:'\\m/', d:"rock on!", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/111.gif" },
		{ k:':-q', d:"thumbs down", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/112.gif" },
		{ k:':o3', d:"puppy dog eyes", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/108.gif" },
		{ k:'%-(', d:"not listening", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/107.gif" },
		{ k:':@)', d:"pig", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/49.gif" },
		{ k:'~:>', d:"chicken", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/52.gif" },
		{ k:'%%-', d:"good luck", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/54.gif" },
		{ k:'~O)', d:"coffee", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/57.gif" },
		{ k:'8-X', d:"skull", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/59.gif" },
		{ k:'=:)', d:"bug", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/60.gif" },
		{ k:'>-)', d:"alien", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/61.gif" },
		{ k:':-L', d:"frustrated", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/62.gif" },
		{ k:'$-)', d:"money eyes", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/64.gif" },
		{ k:':-"', d:"whistling", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/65.gif" },
		{ k:'b-(', d:"feeling beat up", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/66.gif" },
		{ k:'[-X', d:"shame on you", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/68.gif" },
		{ k:'>:/', d:"bring it on", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/70.gif" },
		{ k:';))', d:"hee hee", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/71.gif" },
		{ k:':-@', d:"chatterbox", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/76.gif" },
		{ k:':-j', d:"oh go on", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/78.gif" },
		{ k:'(*)', d:"star", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/79.gif" },
		{ k:'o->', d:"hiro", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/72.gif" },
		{ k:'o=>', d:"billy", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/73.gif" },
		{ k:'o-+', d:"april", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/74.gif" },
		{ k:'(%)', d:"yin yang", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/75.gif" },
		{ k:':bz', d:"bee", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/115.gif" },
		{ k:':)', d:"happy", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/1.gif" },
		{ k:':(', d:"sad", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/2.gif" },
		{ k:';)', d:"winking", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/3.gif" },
		{ k:':D', d:"big grin", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/4.gif" },
		{ k:':x', d:"love struck", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/8.gif" },
		{ k:':P', d:"tongue", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/10.gif" },
		{ k:'X(', d:"angry", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/14.gif" },
		{ k:':>', d:"smug", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/15.gif" },
		{ k:':|', d:"straight face", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/22.gif" },
		{ k:'=;', d:"talk to the hand", u:"http://l.yimg.com/us.yimg.com/i/mesg/emoticons7/27.gif" }
	]
};

