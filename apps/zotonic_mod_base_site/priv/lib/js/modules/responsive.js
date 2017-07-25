/* Support for responsive layouts, adapting behaviour depending on user-agent.
 * 
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
 */
!function( $ ) {
   $(function () {
     $.fn.isMenuCollapsed = function() {
         return $('a[data-toggle="collapse"]:visible', this).length > 0
     };
     
     /* When clicking on collapsed dropdown menu items, just follow the link */
     $('.navbar').delegate('a', 'click', function(ev) {
         if (  $(this).hasClass('dropdown-toggle')
            && $(this).closest(".navbar").isMenuCollapsed()
            && !$(this).attr('href').match(/^#/)) {
                 ev.stopPropagation();
         }
     });
   });
}( window.jQuery );
