/* growl notice js
 ----------------------------------------------------------

 @package:      Zotonic 2009    
 @Author:       Tim Benniks <tim@timbenniks.nl>

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

(function(jQuery)
 {
     jQuery.extend(
     {                    
         noticeAdd: function(options)
         {        
             var defaults = {
                 inEffect:                        {opacity: 'show'},      // in effect
                 inEffectDuration:                300,                            // in effect duration in miliseconds
                 stayTime:                        3000,                           // time in miliseconds before the item has to disappear
                 text:                            '',                                     // content of the item
                 stay:                            false,                          // should the notice item stay or not?
                 type:                            'notice'                        // could also be error, succes
             };

             var map = {
                 notice: 'alert-info',
                 error: 'alert-error'
             };
             
             // declare varaibles
             var options, noticeWrapAll, noticeItemOuter, noticeItemInner, noticeItemClose;
             
             options              = jQuery.extend({}, defaults, options);
             noticeWrapAll        = (!jQuery('.notice-wrap').length) ? jQuery('<div></div>').addClass('notice-wrap').appendTo('body') : jQuery('.notice-wrap');
             noticeItemOuter      = jQuery('<div></div>').addClass('notice-item-wrapper');
             noticeItemInner      = jQuery('<div></div>').hide().addClass('alert  ' + map[options.type]).appendTo(noticeWrapAll).html(options.text).animate(options.inEffect, options.inEffectDuration).wrap(noticeItemOuter);
             noticeItemClose      = jQuery('<a>').addClass('close').prependTo(noticeItemInner).html('&times;').attr("data-dismiss", "alert"); //click(function() { jQuery.noticeRemove(noticeItemInner); });
             
             // hmmmz, zucht
             if(navigator.userAgent.match(/MSIE 6/i)) 
             {
                 noticeWrapAll.css({top: document.documentElement.scrollTop});
             }
             
             if(!options.stay)
             {
                 setTimeout(function()
                            {
                                jQuery.noticeRemove(noticeItemInner);
                            },
                            options.stayTime);
             }
         },
         
         noticeRemove: function(obj)
         {
             obj.animate({opacity: '0'}, 600, function()
                         {
                             obj.parent().animate({height: '0px'}, 300, function()
                                                  {
                                                      obj.parent().remove();
                                                  });
                         });
         }
     });
 })(jQuery);