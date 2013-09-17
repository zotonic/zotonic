
$(function() {
	$('body').on("click", "a", function(evt) {
		var href = $(this).attr('href');
		var m = href && href.match(/\/(..\/)?admin\/edit\/([0-9]+)$/);
		if (m) {
			$('#rscform').mask();
			z_notify("admin-menu-edit", {
						id: m[2],
						tree_id: $('#menu-editor').data('rsc-id'),
						z_delegate: "mod_admin_frontend"
			});
			z_dialog_close();
			evt.preventDefault();
		}
	});

	if (typeof pubzub !== "undefined") {
		pubzub.subscribe("menu/insert", function(topic,args) {
			$('#rscform').mask();
			z_notify("admin-menu-edit", {
						id: args.id,
						tree_id: $('#menu-editor').data('rsc-id'),
						z_delegate: "mod_admin_frontend"
			});
		});
	}
});
