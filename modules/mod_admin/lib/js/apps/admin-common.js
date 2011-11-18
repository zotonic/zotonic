/* Admin Common js
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
	content_css: "/lib/js/modules/tinymce3.4.3.2/zotonic.css", 
	wpeditimage_disable_captions: "", 
	plugins: "paste,table,zlink,zmedia,autosave,directionality,zbdo",
	table_row_limit: 100,
	table_col_limit: 10	
}
