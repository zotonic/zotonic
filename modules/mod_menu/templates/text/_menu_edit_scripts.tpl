{% wire name="admin-menu-select" 
		action={dialog_open 
					template="_action_dialog_connect.tpl" 
					title=(in_sorter == 'category')|if:_"Add category":_"Add menu item"
					callback="window.zMenuEditDone"
					in_sorter=in_sorter}
%}
{% wire name="admin-menu-edit" 
		action={dialog_edit_basics callback="window.zMenuEditDone"} 
%}

{% javascript %}
$('#{{ menu_id }}').on('click', '.menu-edit', function(e) {
	var id = $(this).closest('div').data('page-id');

	window.zMenuEditDone = function(id, title) {
		$(".title-"+id).html(title);
	};
	z_event("admin-menu-edit", { id: id });
	e.preventDefault();
});

$('#{{ menu_id }}').on('click', '.dropdown-menu a', function(e) {
	var where = $(this).data('where');
	var $menu_item = $(this).closest('li.menu-item');
	var $sorter = $('#{{ menu_id }}');

	if (where == 'remove') {
		$(this).closest('li.menu-item').fadeOut(500, function() { 
			$(this).remove();
			$sorter.trigger('sortupdate')
		});
	} else {
		window.zMenuEditDone = function(v) {
			console.log("zMenuEditDone", v);

			window.zMenuNewItem = function(html) {
				console.log(where, html);
				if (where == 'top') {
					$sorter.prepend(html);
				} else if (where == 'bottom') {
					$sorter.append(html);
				} else if (where == 'before') {
					$(html).insertBefore($menu_item);
				} else if (where == 'below') {
					$submenu = $("ul.menu-submenu", $menu_item);
					if ($submenu.length > 0) {
						$submenu.append(html);
					} else {
						$menu_item.append("<ul class='.menu-submenu'>"+html+"</ul>");
					}
				} else if (where == 'after') {
					$(html).insertAfter($menu_item);
				}
				$sorter.trigger('sortupdate')
			};

			z_notify("menu-item-render", {id: v.object_id, callback: "window.zMenuNewItem", z_delegate:"mod_menu"});
		};
		z_event("admin-menu-select");
	}
	e.preventDefault();
});


{% endjavascript %}

