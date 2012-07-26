<div class="widget" id="{{ #menu }}">
	<h3 class="widget-header">
		{_ Current menu _}
	</h3>
	<div class="widget-content">

        <span class="btn-group pull-right">
            <a href="#" class="btn dropdown-toggle" data-toggle="dropdown">{_ Add menu item _} <span class="caret"></span></a>
            <ul class="dropdown-menu">
                <!-- dropdown menu links -->
                <li><a href="#" data-where="top">&uarr; {_ Add top _}</a></li>
                <li><a href="#" data-where="bottom">&darr; {_ Add bottom _}</a></li>
            </ul>
        </span>

        <p>
            {_ Click on <strong>Add menu item</strong> or <strong>Menu item</strong> to add pages. _}
            <br/>{_ Drag menu items in the menu up, down, left or right to structure the menu. _}
        </p>


		<ul class="tree-list do_menuedit" id="menu-{{ id }}">
			{% for mid, path, action in id.menu|menu_flat %}
			{% with forloop.counter as c %}
				{% include "_menu_edit_item.tpl" c=forloop.counter id=mid %}
			{% endwith %}
			{% endfor %}
		</ul>
	</div>
</div>

{% wire name="admin-menu-select" 
		action={dialog_open 
					template="_action_dialog_connect.tpl" 
					title=_"Add menu item"
					callback="window.zMenuEditDone"}
%}
{% wire name="admin-menu-edit" 
		action={dialog_edit_basics callback="window.zMenuEditDone"} 
%}

{% javascript %}
$('#{{ #menu }}').on('click', '.menu-edit', function(e) {
	var id = $(this).closest('div').data('page-id');

	window.zMenuEditDone = function(id, title) {
		$(".title-"+id).html(title);
	};
	z_event("admin-menu-edit", { id: id });
	e.preventDefault();
});

$('#{{ #menu }}').on('click', '.dropdown-menu a', function(e) {
	var where = $(this).data('where');
	var $menu_item = $(this).closest('li.menu-item');
	var $sorter = $('#menu-{{ id }}');

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

