/* =============================================================
 * scrollpaluza.js
 * =============================================================
 * Copyright 2012 Marc Worrell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ============================================================== */

!function ($) {
    $(function () {
      var targets = [];
      $('h1,h2,h3,h4,h5,h6,blockquote,section,div').each(function() {
          var id = $(this).attr('id');
          if (id) {
              targets.push({top:$(this).position().top, id:id});
          }
      });
      
      var waiting = false;
      $(document).scroll(function() {
          if (!waiting) {
              waiting = true;
              setTimeout(function() { 
                  var bestd, best=false, top=$(window).scrollTop();
                  for (i=targets.length-1; i>=0; i--) {
                      var d = Math.abs(top - targets[i].top);
                      if (d < 200 && (!best || d < bestd)) {
                          best = targets[i];
                          bestd = d;
                      }
                  }
                  if (best) {
                      document.location.hash = '#'+best.id;
                  }
                  waiting=false;
                }, 150);
          }
      });
    });
}(window.jQuery);
