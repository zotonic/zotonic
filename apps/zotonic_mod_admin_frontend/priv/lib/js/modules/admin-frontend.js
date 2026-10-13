
$(function() {
    const menu = document.getElementById("menu-editor");
    const menuTemplate = document.getElementById("admin-frontend-menu-button");
    const toolbar = document.getElementById("save-buttons");
    if (menu && menuTemplate && toolbar) {
        const layout = menu.parentElement;
        const button = menuTemplate.content.firstElementChild.cloneNode(true);
        toolbar.prepend(button);
        document.body.classList.add("admin-frontend-editor");
        layout.classList.add("admin-frontend-layout");

        function showMenu(show) {
            document.body.classList.toggle("admin-frontend-menu-open", show);
            button.setAttribute("aria-expanded", String(show));
        }
        button.addEventListener("click", function() {
            showMenu(button.getAttribute("aria-expanded") !== "true");
        });
        // Observe the editor's available width, including changes from the preview
        // divider. Viewport breakpoints alone miss a narrow editor on a wide screen.
        new ResizeObserver(function(entries) {
            const compact = entries[0].contentRect.width < 1100;
            if (compact !== document.body.classList.contains("admin-frontend-compact")) {
                showMenu(false);
                document.body.classList.toggle("admin-frontend-compact", compact);
            }
        }).observe(layout);
        window.addEventListener("hashchange", function() { showMenu(false); });
    }

	function confirm_unsaved( hash ) {
		z_editor_save($('#rscform'));
		if ($('#rscform').attr('data-formdirty')) {
			z_dialog_confirm({
				cancel: z_translate("Yes, discard changes"),
				ok: z_translate("Cancel"),
				text: '<p>' + z_translate("There are unsaved changes. Are you sure you want to leave without saving?") + '</p>',
				on_cancel: function() {
					window.location.hash = hash;
				}
			});
		} else {
			window.location.hash = hash;
		}
	}

	$('body').on("click", "a", function(evt) {
		var href = $(this).attr('href');
		var m = href && href.match(/\/(..\/|..-[a-z]+\/)?admin\/edit\/([0-9]+)$/);
		if (m) {
			confirm_unsaved("#edit_id="+m[2]);
			evt.preventDefault();
		}
	});

	z_event_register("admin-menu-edit", function(args) {
		let id = "";
		let tab = "";

		for (var i in args) {
			switch (args[i]["name"]) {
				case "id":
					id = args[i].value;
					break;
				case "tab":
					tab = args[i].value;
					break;
				default:
					break;
			}
		}
		if (id) {
			confirm_unsaved("#edit_id="+encodeURIComponent(id) + "&tab=" + encodeURIComponent(tab));
		}
	});

	function hashchange( id ) {
		if (window.location.hash) {
			z_dialog_close(0);
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

			if (args["edit_id"]) {
				z_notify("admin-menu-edit", {
							id: args["edit_id"],
							tab: args["tab"],
							tree_id: $('#menu-editor').data('rsc-id'),
							z_delegate: "mod_admin_frontend"
				});
			}
		} else if (id) {
			z_notify("admin-menu-edit", {
						id: id,
						tree_id: $('#menu-editor').data('rsc-id'),
						z_delegate: "mod_admin_frontend"
			});
		}
	}

	window.addEventListener("hashchange", function() {
		hashchange(undefined);
	}, false);

	cotonic.ready.then( function() {
		cotonic.broker.subscribe("menu/insert", function(msg) {
			confirm_unsaved("#edit_id="+msg.payload.id);
		});

		cotonic.broker.subscribe("menu/edit-default", function(msg) {
			setTimeout(function() { hashchange(msg.payload.id) }, 0);
		});
	});

});
