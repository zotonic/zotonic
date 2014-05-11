/**
 * $Id: editor_plugin.js 677 2008-03-07 13:52:41Z spocke $
 *
 * @author Moxiecode
 * @copyright Copyright © 2004-2008, Moxiecode Systems AB, All rights reserved.
 */


(function() 
{
	var DOM = tinymce.DOM;

	tinymce.create('tinymce.plugins.ZotonicMorePlugin', 
	{
		init: function(ed, url) 
		{
			var t = this;
			var tbId = ed.getParam('zotonic_more_toolbar', 'toolbar2')
			
			// Hides the specified toolbar and resizes the iframe
			ed.onPostRender.add(function() 
			{
				if(ed.getParam('zotonic_more_toolbar', 1))
				{
					DOM.hide(ed.controlManager.get(tbId).id);
					t._resizeIframe(ed, tbId, 28);
				}
			});
					
			ed.addCommand('Z_More', function() 
			{
				var id = ed.controlManager.get(tbId).id, cm = ed.controlManager;

				if(DOM.isHidden(id)) 
				{
					cm.setActive('zmore', 1);
					DOM.show(id);
					t._resizeIframe(ed, tbId, -28);
					ed.settings.zotonic_more_toolbar = 0;
				} 
				else 
				{
					cm.setActive('zmore', 0);
					DOM.hide(id);
					t._resizeIframe(ed, tbId, 28);
					ed.settings.zotonic_more_toolbar = 1;
				}
			});
			
			ed.addButton('zmore', {
				title: 'Show more',
				cmd: 'Z_More',
				'class': 'mce_zmore'
			});
		},

		getInfo : function() {
			return {
				longname : 'Zotonic more Plugin',
				author : 'Tim Benniks',
				authorurl : 'http://www.zotonic.com',
				infourl : 'http://www.zotonic.com',
				version : tinymce.majorVersion + "." + tinymce.minorVersion
			};
		},
			
		// Resizes the iframe by a relative height value
		_resizeIframe : function(ed, tb_id, dy) {
			var ifr = ed.getContentAreaContainer().firstChild;

			DOM.setStyle(ifr, 'height', ifr.clientHeight + dy); // Resize iframe
			ed.theme.deltaHeight += dy; // For resize cookie
		}
	});

	// Register plugin
	tinymce.PluginManager.add('zmore', tinymce.plugins.ZotonicMorePlugin);
})();
