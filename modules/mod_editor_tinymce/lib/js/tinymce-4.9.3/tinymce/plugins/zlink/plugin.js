/**
 * Create internal links to zotonic pages in the body text, by using the typeahead.
 *
 * @author Arjan Scherpenisse <arjan@scherpenisse.net>
 * @copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
 */

tinymce.PluginManager.requireLangPack('zlink');

tinymce.PluginManager.add('zlink', function(editor, url) {

    var showLinkDialog = function() {
        window.z_zlink = function(url, title) {
            var linkAttrs = {
                    href: url,
                    title: title
                },
                dom = editor.dom,
                selection = editor.selection;
            if (selection.getContent()) {
                editor.execCommand('mceInsertLink', false, linkAttrs);
            } else {
                editor.insertContent(dom.createHTML('a', linkAttrs, dom.encode(title)));
            }
        }
        z_event('zlink', {language: window.zEditLanguage()});
    };
    
    // Add a button to the button bar
	editor.addButton('zlink', {
		title: 'Insert an internal link',
		icon: 'link',
		onclick: showLinkDialog
	});

	// Add a menu item to the tools menu
	editor.addMenuItem('zlink', {
		icon: 'link',
		text: 'Insert internal link',
		context: 'format',
		onclick: showLinkDialog
	});
});