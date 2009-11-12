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
	mode: "exact",
	elements: "field-content",
	theme: "advanced",
	skin: "wp_theme", 
	theme_advanced_buttons1: "bold,italic,|,bullist,numlist,blockquote,|,justifyleft,justifycenter,justifyright,|,link,unlink,|,code,|,formatselect,|,zmedia,zmore",
	theme_advanced_buttons2: "tablecontrols",
	theme_advanced_buttons3: "",
	theme_advanced_buttons4: "",
	theme_advanced_toolbar_location: "top", 
	theme_advanced_toolbar_align: "left", 
	theme_advanced_statusbar_location: "bottom", 
	theme_advanced_resizing: "1", 
	theme_advanced_resize_horizontal: "",
	theme_advanced_blockformats: "pre,h1,h2,h3,h4", 
	dialog_type: "modal", 
	relative_urls: "", 
	remove_script_host: "", 
	convert_urls: "", 
	apply_source_formatting: "", 
	remove_linebreaks: "1", 
	paste_convert_middot_lists: "1", 
	paste_remove_spans: "1", 
	paste_remove_styles: "1", 
	gecko_spellcheck: "1", 

	/* below is a workaround for problem where tinyMCE setEntities skips the ones below and the doesn't initialize the entityLookup array
	 * which results in an error in the _encode function.
	 */
	entity_encoding: "raw",     
	entities: "38,amp,60,lt,62,gt", 

	accessibility_focus: "1", 
	tab_focus: ":prev,:next", 
	content_css: "/lib/js/modules/tinymce/zotonic.css", 
	wpeditimage_disable_captions: "", 
	plugins: "safari,table,zmedia,zmore",
	table_row_limit: 100,
	table_col_limit: 10
}

$('.do_datepicker').datepicker(
{ 
	dateFormat: 'yy-mm-dd',
	showAnim: 'fadeIn'
});