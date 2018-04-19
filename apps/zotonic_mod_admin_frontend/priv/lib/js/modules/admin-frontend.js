
$(function() {
	$('body').on("click", "a", function(evt) {
		var href = $(this).attr('href');
		var m = href && href.match(/\/(..\/)?admin\/edit\/([0-9]+)$/);
		if (m) {
			window.location.hash = "#edit_id="+m[2];
			evt.preventDefault();
		}
	});

	// if (typeof pubzub !== "undefined") {
	// 	pubzub.subscribe("~pagesession/menu/insert", function(topic,args) {
	// 		window.location.hash = "#edit_id="+args.id;
	// 	});
	// }

	z_event_register("admin-menu-edit", function(args) {
		for (var i in args) {
			if (args[i]["name"] == "id") {
				window.location.hash = "#edit_id="+args[i].value;
			}
		}
	});

	function hashchange()
	{
		if (window.location.hash) {
			z_dialog_close();
			$('#rscform').mask();

			var query = window.location.hash.substring(1);
			var vars = query.split('&');
			var args = {};
			for (var i = 0; i < vars.length; i++) {
				var pair = vars[i].split('=');
				if (pair.length == 2) {
					args[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);
				}
			}
			z_notify("admin-menu-edit", {
						id: args["edit_id"],
						tree_id: $('#menu-editor').data('rsc-id'),
						z_delegate: "mod_admin_frontend"
			});
		} else if (typeof document.z_default_edit_id == 'number') {
			z_notify("admin-menu-edit", {
						id: document.z_default_edit_id,
						tree_id: $('#menu-editor').data('rsc-id'),
						z_delegate: "mod_admin_frontend"
			});
		}
	}

	window.addEventListener("hashchange", function() {
		hashchange();
	}, false);

	setTimeout(function() { hashchange(); }, 100);
});
