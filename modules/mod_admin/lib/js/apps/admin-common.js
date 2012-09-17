/* Admin Common js
----------------------------------------------------------

@package:	Zotonic 2009
@Author:	Tim Benniks <tim@timbenniks.nl>

Copyright 2009 Tim Benniks
Copyright 2012 Arjan Scherpenisse

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

(function($)
{
    $.widget("ui.adminLinkedTable",
    {
        _init: function() {
            var self = this;
            (self.element.find("tr").each(function() {
                                              var href = $(this).attr("data-href");
                                              if (!href) return;
                                              $(this).find("td").each(function() {
                                                                          $(this)
                                                                              .addClass("view-link")
                                                                              .contents()
                                                                              .wrapAll($("<a>")
                                                                              	.addClass("view-link")
                                                                              	.attr("href", href));
                                                                      });
                                          }));
        }
    });

    $.widget("ui.autofocus",
    {
        _init: function() {
            var self = this;
            self.element.focus();
        }
    });
    
    $(window).scroll(function() {
        if ($(window).scrollTop() > 118) {
            $('body').addClass('scrolled');
        } else {
            $('body').removeClass('scrolled');
        }
    });
})(jQuery);


window.zAdminConnectDone = function(v) {
	if (v.is_new) {
		var target_id = "links-"+v.subject_id+"-"+v.predicate;
		var $elt = $("#"+target_id);
		$elt.mask("", 10);
		z_notify("update", {
			z_delegate: "mod_admin",
			z_target_id: target_id,
			z_trigger_id: target_id,
			id: v.subject_id,
			predicate: v.predicate,
			template: $elt.data("reload-template")
		});
	}
};

window.zAdminLinkDone = function(v) {
	window.z_zlink(v.url_language, v.title_language);
	window.zAdminConnectDone(v);
}

window.zAdminMediaDone = function(v) {
	window.z_choose_zmedia(v.object_id);
	window.zAdminConnectDone(v);
}

window.zEditLanguage = function() {
	return $('.language-tabs li.active').attr('lang');
}



function z_admin_ensure_block_names() {
    var names = [];
    $('.blocks input.block-name').each(function() { 
    	var name = $(this).val();
    	if (name != '') {
    		names.push(name);
    	}
    });

    $('.blocks input.block-name').each(function() { 
    	var name = $(this).val();
    	if (name == '')
    	{
    		var $block = $(this).closest(".block");
    		name = $("input.block-type", $block).val().split("_")[0]
    		var ct = 1;
		    while (names.indexOf(name+ct) != -1) {
		        ct++;
		    }
		    $("input.block-name", $block).val(name+ct);
	        names.push(name+ct);
    	}
    });
}

