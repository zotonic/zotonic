/* listfilter js
----------------------------------------------------------

@package: Zotonic 2009
@Author:  Tim Benniks <tim@timbenniks.nl>

Copyright 2009-2023 Tim Benniks

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

qs_score - Quicksilver Score

A port of the Quicksilver string ranking algorithm

"hello world".score("axl") //=> 0.0
"hello world".score("ow") //=> 0.6
"hello world".score("hello world") //=> 1.0

---------------------------------------------------------- */

String.prototype.score = function(abbreviation, offset) {
  offset = offset || 0 // TODO: I think this is unused... remove

  if(abbreviation.length == 0) return 0.9
  if(abbreviation.length > this.length) return 0.0

  for (let i = abbreviation.length; i > 0; i--) {
    const sub_abbreviation = abbreviation.substring(0,i)
    const index = this.indexOf(sub_abbreviation)

    if (index < 0) continue;
    if (index + abbreviation.length > this.length + offset) continue;

    const next_string     = this.substring(index+sub_abbreviation.length);
    let next_abbreviation = null;

    if(i >= abbreviation.length) {
      next_abbreviation = '';
    } else {
      next_abbreviation = abbreviation.substring(i);
    }

    const remaining_score   = next_string.score(next_abbreviation,offset+index)

    if (remaining_score > 0) {
      let score = this.length-next_string.length;

      if (index != 0) {
        let c = this.charCodeAt(index-1)

        if (c == 32 || c == 9) {
          for (let j=(index-2); j >= 0; j--) {
            c = this.charCodeAt(j)
            score -= ((c == 32 || c == 9) ? 1 : 0.15)
          }
        } else {
          score -= index
        }
      }
      score += remaining_score * next_string.length
      score /= this.length;
      return score
    }
  }
  return 0.0
}

jQuery.fn.listfilter = function(options)
{
    var method = "words";

    if (typeof options.method == "string") {
        method = options.method;
    }
    this.keyup(filter)
          .keyup()
          .parents('form')
          .submit(function() { return false; });
    return this;

    function filter()
    {
        const rows = $(options.list);
        const term = $.trim($(this).val().toLowerCase());
        const words = term.match(/\b(\w+)\b/g);
        let scores = [];

        if(!term) {
            rows.removeClass('hide');
        } else {
            rows.addClass('hide');
            rows.each(function(i) {
                let text;

                if (typeof options.text == 'string') {
                    text = $(options.text, this).text().toLowerCase();
                } else {
                    text = $(this).text().toLowerCase();
                }
                switch (method) {
                    case 'score':
                        const score = text.score(term);
                        if(score > 0) {
                            $(this).removeClass('hide');
                        }
                        break;
                    default:
                        // words
                        let is_match = true;
                        words.forEach((w) => {
                            is_match = is_match && (text.indexOf(w) >= 0);
                        });
                        if (is_match) {
                            $(this).removeClass('hide');
                        }
                        break;
                }
            });
        }
        jQuery(this).trigger("z.listfilter:done");
    }
};
