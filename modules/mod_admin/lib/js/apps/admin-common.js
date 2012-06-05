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
                                                                              .wrapAll($("<a>").addClass("view-link").attr("href", href));
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
})(jQuery);



tinyInit = {
	mode: "none",
	theme: "advanced",
	theme_advanced_buttons1: "bold,italic,|,bullist,numlist,blockquote,|,justifyleft,justifycenter,justifyright,|,link,unlink,|,cleanup,code,|,formatselect,|,zlink,zmedia",
	theme_advanced_buttons2: "tablecontrols,outdent,indent",
	theme_advanced_buttons3: "",
	theme_advanced_buttons4: "",
	theme_advanced_toolbar_location: "top", 
	theme_advanced_toolbar_align: "left", 
	theme_advanced_statusbar_location: "bottom", 
	theme_advanced_resizing: "1", 
	theme_advanced_resize_horizontal: "",
	theme_advanced_blockformats: "p,h1,h2,h3,h4,pre",
	dialog_type: "modal", 
	relative_urls: "", 
	remove_script_host: "", 
	convert_urls: "", 
	apply_source_formatting: "", 
	remove_linebreaks: "1", 
	gecko_spellcheck: "1", 
	
	formats : {
		bdo_rtl : {inline : 'bdo', attributes : {dir: 'rtl'}},
		bdo_ltr : {inline : 'bdo', attributes : {dir: 'ltr'}}
	},

	/* Cleanup pasted html code */
	paste_auto_cleanup_on_paste : true,
	paste_convert_middot_lists: true, 
	paste_remove_spans: true,
	paste_remove_styles: true,
	paste_remove_styles_if_webkit: true,
	paste_strip_class_attributes: true,

	/* below is a workaround for problem where tinyMCE setEntities skips the ones below and the doesn't initialize the entityLookup array
	 * which results in an error in the _encode function.
	 */
	entity_encoding: "raw",		
	entities: "38,amp,60,lt,62,gt", 

	accessibility_focus: "1", 
	tab_focus: ":prev,:next", 
	content_css: "/lib/js/modules/tinymce3.5.0/zotonic.css", 
	wpeditimage_disable_captions: "", 
	plugins: "paste,table,zlink,zmedia,autosave,directionality,zbdo",
	table_row_limit: 100,
	table_col_limit: 10,

	/* Adapted valid element list, added some html5 elements, removed controls, object, embed etc */
	/* See: http://www.tinymce.com/wiki.php/Configuration:valid_elements */
	valid_elements : "@[class|style|title|dir<ltr?rtl|lang|xml::lang],"
	+ "a[rel|rev|charset|hreflang|tabindex|accesskey|type|name|href|target|title],"
	+ "strong/b,em/i,strike,u,"
	+ "#p,-ol[type|compact],-ul[type|compact],-li,br,img[longdesc|usemap|"
	+ "src|border|alt=|title|hspace|vspace|width|height|align],-sub,-sup,"
	+ "-blockquote,-table[border=0|cellspacing|cellpadding|width|frame|rules|"
	+ "height|align|summary|bgcolor|background|bordercolor],-tr[rowspan|width|"
	+ "height|align|valign|bgcolor|background|bordercolor],tbody,thead,tfoot,"
	+ "#td[colspan|rowspan|width|height|align|valign|bgcolor|background|bordercolor|scope],"
	+ "#th[colspan|rowspan|width|height|align|valign|scope],caption,-div,"
	+ "-span,-code,-pre,address,-h1,-h2,-h3,-h4,-h5,-h6,hr[size|noshade],"
	+ "dd,dl,dt,cite,abbr,del[datetime|cite],ins[datetime|cite],"
	+ "map[name],area[shape|coords|href|alt|target],bdo,"
	+ "col[align|char|charoff|span|valign|width],colgroup[align|char|charoff|span|valign|width],"
	+ "dfn,kbd,legend,"
	+ "q[cite],samp,small,"
	+ "tt,var,big,"
	+ "section,header,nav,article,footer,audio,video"
};
