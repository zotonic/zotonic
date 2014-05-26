/**
 * Create internal links to zotonic pages in the body text, by using the typeahead.
 *
 * @author Arjan Scherpenisse <arjan@scherpenisse.net>
 * @copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
 */


(function() 
{
	var DOM = tinymce.DOM;

	tinymce.create('tinymce.plugins.ZotonicLinkPlugin', 
	{
		init: function(ed, url) 
		{
			ed.addCommand('Z_Link', function() 
			{
	            window.z_zlink = function(url, title) {
	                //var html = id + "<b>{$selection}</b>";
	                if (ed.selection.getContent())
	                {
	                    ed.execCommand('mceInsertLink', false, {href: url, title: title});
	                }
	                else
	                {
	                    var html = "<a href=\"" + url + "\">" + title + "</a>";
	                    ed.execCommand('mceInsertContent', false, html);
	                }
				};
                z_event('zlink', {language: window.zEditLanguage()});
			});

			ed.addButton('zlink', {
				title: 'Add internal link',
				cmd: 'Z_Link',
				'class': 'mce_zlink'
			});
		},

		getInfo : function() {
			return {
				longname : 'Zotonic Internal Link Plugin',
				author : 'Arjan Scherpenisse',
				authorurl : 'http://www.zotonic.com',
				infourl : 'http://www.zotonic.com',
				version : '0.1'
			};
		}
	});

	// Register plugin
	tinymce.PluginManager.add('zlink', tinymce.plugins.ZotonicLinkPlugin);
})();
