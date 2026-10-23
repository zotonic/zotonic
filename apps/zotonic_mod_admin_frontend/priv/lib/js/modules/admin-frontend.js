
$(function() {
    const menu = document.getElementById("menu-editor");
    const menuTemplate = document.getElementById("admin-frontend-menu-button");
    const toolbar = document.getElementById("save-buttons");
    const editor = document.getElementById("editcol");
    const editorPane = document.getElementById("editcol-wrapper") || editor;
    const layout = editorPane && editorPane.parentElement;
    if (layout) {
        document.body.classList.add("admin-frontend-editor");
        layout.classList.add("admin-frontend-layout");
        layout.classList.toggle("admin-frontend-without-menu", !menu);

        // Site templates have different headers. Reserve their actual space,
        // including toolbar wrapping when the preview reduces the editor width.
        const navbar = document.querySelector("nav.navbar-fixed-top");
        const updateHeight = function() {
            const top = Math.max(layout.getBoundingClientRect().top,
                navbar ? navbar.getBoundingClientRect().bottom : 0);
            layout.style.setProperty("--admin-frontend-top", Math.max(0, top) + "px");
        };
        updateHeight();
        if (navbar) new ResizeObserver(updateHeight).observe(navbar);
        window.addEventListener("resize", updateHeight);

        // Form updates stay inside the stable scroll pane. Only a new resource
        // resets its position; saves and language changes retain it.
        let resourceId;
        cotonic.ready.then(function() {
            cotonic.broker.subscribe("admin/preview/context", function(msg) {
                if (resourceId !== msg.payload.id) editorPane.scrollTop = 0;
                resourceId = msg.payload.id;
            });
        });
    }
    if (menu && menuTemplate && toolbar && layout) {
        const button = menuTemplate.content.firstElementChild.cloneNode(true);
        toolbar.prepend(button);
        document.body.classList.add("admin-frontend-editor");
        layout.classList.add("admin-frontend-layout");

        function showMenu(show) {
            document.body.classList.toggle("admin-frontend-menu-open", show);
            button.setAttribute("aria-expanded", String(show));
            button.classList.toggle("btn-primary", show);
            button.classList.toggle("btn-default", !show);
        }
        button.addEventListener("click", function() {
            showMenu(button.getAttribute("aria-expanded") !== "true");
        });
        // Use the window space left beside the preview, independent of a site's
        // editor container widths. Observe the panel for opening and resizing.
        const preview = document.getElementById("admin-edit-preview");
        function updateCompact() {
            const previewWidth = preview && !preview.hidden
                ? preview.getBoundingClientRect().width
                : 0;
            const compact = window.innerWidth - previewWidth < 1100;
            if (compact !== document.body.classList.contains("admin-frontend-compact")) {
                showMenu(false);
                document.body.classList.toggle("admin-frontend-compact", compact);
            }
        }
        if (preview) new ResizeObserver(updateCompact).observe(preview);
        window.addEventListener("resize", updateCompact);
        updateCompact();
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
