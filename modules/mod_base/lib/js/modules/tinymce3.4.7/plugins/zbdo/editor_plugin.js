/**
 * Change the direction of the current selection.
 *
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2011 Marc Worrell <marc@worrell.nl>
 */


(function() 
{
	var DOM = tinymce.DOM;

	tinymce.create('tinymce.plugins.ZotonicBdoPlugin', {
		init: function(ed, url) {
			ed.addCommand('Z_bdo_ltr', function() 
			{
				ZBDO.toggle_dir(ed, "ltr");
			});
			ed.addCommand('Z_bdo_rtl', function() 
			{
				ZBDO.toggle_dir(ed, "rtl");
			});

			ed.addButton('zbdoltr', {
				title: 'Left to right',
				cmd: 'Z_bdo_ltr',
				'class': 'mce_zbdoltr'
			});
			ed.addButton('zbdortl', {
				title: 'Right to left',
				cmd: 'Z_bdo_rtl',
				'class': 'mce_zbdortl'
			});

			ed.onNodeChange.add(function(ed, cm, n, co) {
				n = ed.dom.getParent(n, 'BDO');
				cm.setActive('zbdoltr', 0);
				cm.setActive('zbdortl', 0);
				if (n) {
					if (n.getAttribute("dir") == "rtl")
						cm.setActive('zbdortl', 1);
					else
						cm.setActive('zbdoltr', 1);
				}
			});

			ed.onPreInit.add(function() {
				// Fixed IE issue where it can't handle these elements correctly
				ed.dom.create('bdo');
			});
		},

		getInfo : function() {
			return {
				longname : 'Zotonic Inline Text Direction Plugin',
				author : 'Marc Worrell',
				authorurl : 'http://www.zotonic.com/',
				infourl : 'http://www.zotonic.com/',
				version : '0.1'
			};
		}
	});

	// Register plugin
	tinymce.PluginManager.add('zbdo', tinymce.plugins.ZotonicBdoPlugin);

	var ZBDO = {};
	ZBDO.toggle_dir = function(ed, dir) {
		var focusElement = ed.selection.getNode();
		var dom = ed.dom;
		var elm = dom.getParent(focusElement, 'BDO');

		if (elm == null) {
			var s = ed.selection.getContent();
			if(s.length > 0) {
				ed.getDoc().execCommand('FontName', false, 'mceinline');
				tinymce.each(dom.select('span,font'), function(n) {
						if (n.style.fontFamily == 'mceinline' || n.face == 'mceinline')
							dom.replace(dom.create('BDO', {'dir' : dir}), n, 1);
					});
			} else {
				ed.formatter.apply('bdo_' + dir);
			}
		} else {
			if (elm.getAttribute("dir") == dir) {
				ed.execCommand('mceRemoveNode', false, elm);
			} else {
				elm.setAttribute("dir", dir);
			}
		}
		ed.nodeChanged();
	}

})();
