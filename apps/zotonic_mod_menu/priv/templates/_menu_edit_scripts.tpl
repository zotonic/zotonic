{% if in_sorter == 'category' %}
	{# Category - items can only appear once, strict handling #}
    {% wire name="admin-menu-select"
            action={
                dialog_open
                intent="select"
                template="_action_dialog_connect.tpl"
                title=_"Add category"
                callback="window.zMenuEditDone"
                category=`category`
                in_sorter=in_sorter
                tabs_enabled=["new"]
                center=0
                autoclose
                width="large"
            }
    %}
{% elseif is_hierarchy %}
	{# Hierarchy - items can only appear once #}
    {% wire name="admin-menu-select"
            action={
                dialog_open
                intent="select"
                template="_action_dialog_connect.tpl"
                title=_"Add item"
                callback="window.zMenuEditDone"
                category=cat_id
                nocatselect
                in_sorter=in_sorter
                center=0
                autoclose
                width="large"
            }
    %}
{% else %}
	{# Menu - items can appear multiple times #}
    {% wire name="admin-menu-select"
            action={dialog_open
                intent="select"
                template="_action_dialog_connect.tpl"
                catinclude
                id=tree_id
                title=_"Add menu item"
                callback="window.zMenuEditDone"
                category=cat_id
                in_sorter=in_sorter
                center=0
                autoclose
                width="large"
                is_zlink
            }
    %}
{% endif %}

{% if admin_menu_edit_action /= `none` %}
	{% wire
	    name="admin-menu-edit"
        action=admin_menu_edit_action|default:{
            dialog_edit_basics
            callback="window.zMenuEditDone"
        }
	%}
{% endif %}


{% javascript %}

window.menu_insert_queue = [];
window.menu_is_busy = false;


function menu_dequeue()
{
    if (!menu_is_busy && menu_insert_queue.length > 0) {
        let msg = menu_insert_queue.shift();
        let $menu_item;
        let parent_id = msg.payload.parent_id;
        let sub_id = msg.payload.sub_id;


        if (msg.payload.menu_item) {
            $menu_item = msg.payload.menu_item;
        } else {
            $menu_item = $('#{{ menu_id }} ul.tree-list li [data-page-id='+parent_id+']').closest(".menu-item");
        }

        switch (msg.cmd) {
            case "insert-below":
                let $sorter = $('#{{ in_sorter }}');
                let options = $sorter.data().uiMenuedit.options;

                window.zMenuNewItem = function(rsc_id, html) {
                    $submenu = $(">ul", $menu_item);
                    if ($submenu.length > 0) {
                        $submenu.append(html);
                    } else {
                        $menu_item.append("<ul>"+html+"</ul>");
                    }
                    $menu_item
                        .addClass("has-submenu")
                        .addClass("submenu-open");
                    $sorter.trigger('sortupdate');

                    menu_is_busy = false;
                    menu_dequeue();
                };

                menu_is_busy = true;
                z_transport("mod_menu", "ubf", {
                        cmd: "menu-item-render",
                        id: sub_id,
                        callback: "window.zMenuNewItem",
                        template: options.item_template || ""
                    });
                break;
            case "menu-edit-done":
                let where = msg.payload.where;
                let object_id = msg.payload.object_id;

                window.zMenuNewItem = function(rsc_id, html) {
                    if (where == 'top') {
                        msg.payload.sorter.prepend(html);
                    } else if (where == 'bottom') {
                        msg.payload.sorter.append(html);
                    } else if (where == 'before') {
                        $(html).insertBefore($menu_item);
                    } else if (where == 'below') {
                        $submenu = $(">ul", $menu_item);
                        if ($submenu.length > 0) {
                            $submenu.append(html);
                        } else {
                            $menu_item.append("<ul>"+html+"</ul>");
                        }
                        msg.payload.menu_item
                            .addClass("has-submenu")
                            .addClass("submenu-open");
                    } else if (where == 'after') {
                        $(html).insertAfter($menu_item);
                    }
                    msg.payload.sorter.trigger('sortupdate');
                    cotonic.broker.publish("menu/insert", {
                        menu_id: '{{ menu_id }}',
                        id: rsc_id
                    });

                    menu_is_busy = false;
                    menu_dequeue();
                };

                {% if is_hierarchy or in_sorter == 'category' %}
                    var $duplicate = msg.payload.sorter.find('[data-page-id='+object_id+']');
                    if ($duplicate.length > 0) {
                        z_dialog_alert({text: "{_ This item is already in the hierarchy. Every item can only occur once. _}"});
                        $duplicate.fadeTo(500, 0.5, function() { $duplicate.fadeTo(500, 1); });
                        return;
                    }
                {% endif %}

                menu_is_busy = true;
                z_transport("mod_menu", "ubf", {
                        cmd: "menu-item-render",
                        id: object_id,
                        callback: "window.zMenuNewItem",
                        template: msg.payload.options.item_template || ""
                    });

                break;
            default:
                console.log("Unknown menu queue command");
                break;
        }
    }
}


cotonic.ready.then(
    function() {
    	var sortupdater = undefined;

    	cotonic.broker.subscribe("bridge/origin/model/rsc/event/+id/delete", function(args, bindings) {
            var id = parseInt(bindings.id);
    		var elts = $('#{{ menu_id }} [data-page-id='+bindings.id+']').closest('li.menu-item');
    		if (elts.length) {
    			elts.remove();
    			if (sortupdater) {
    				clearTimeout(sortupdater);
    			}
    			sortupdater = setTimeout(function() {
    				$('#{{ in_sorter }}').trigger('sortupdate');
    			}, 150);
    		}
    	});

        cotonic.broker.subscribe("menu/edit", function(msg, bindings) {
            z_event("admin-menu-edit", { id: msg.payload.id, tree_id: {{ tree_id|default:"undefined" }} });
        });

        cotonic.broker.subscribe("menu/edit/insert-below", function(msg, bindings) {
            let m = {
                cmd: "insert-below",
                payload: msg.payload
            }
            menu_insert_queue.push(m);
            menu_dequeue();
        });
    });

$('#{{ menu_id }}').on('click', '.menu-edit', function(e) {
	var id = $(this).closest('.menu-wrapper').data('page-id');
	window.zMenuEditDone = function(id, title) {
		$(".title-"+id+" .menu-label").html(title);
	};
	z_event("admin-menu-edit", { id: id, tree_id: {{ tree_id|default:"undefined" }} });
	e.preventDefault();
});

$('#{{ menu_id }}').on('click', '.menu-toggle', function(e) {
	e.stopImmediatePropagation();
	$(this).closest('li').toggleClass('submenu-open');
});

$('#{{ menu_id }}').on('click', '.dropdown-menu a', function(e) {
	var $a = $(e.currentTarget);
	var where = $a.data('where');
	var $menu_item = $a.closest('li.menu-item');
	var $sorter = $('#{{ in_sorter }}');
	var $menuedit = $a.closest(".do_menuedit");
    var options = $sorter.data().uiMenuedit.options;

	if ($menuedit.length === 0) {
		$menuedit = $(".do_menuedit", $a.closest("div"));
	}
	if ($menuedit.length === 0) {
		$menuedit = $(".do_menuedit", $a.closest(".widget"));
	}

	if (where == 'remove') {
        let page_id = $menu_item.children('div').data('page-id');

		{% if is_hierarchy or in_sorter == 'category' %}
			z_transport('mod_menu', 'ubf', {
				cmd: "delete",
				id: page_id
			});
		{% else %}
            let self = this;

            z_dialog_confirm({
                text: "{_ Are you sure you want to delete _}: "
                    + "<b>" + $menu_item.find(".menu-label").html() + "</b><br>"
                    + "{_ and all indented items below it from the menu? _}"
                    + "<b id=\"menu-delete-undo-text\" style=\"display:none\"><br><br>{_ THIS CAN NOT BE UNDONE! _}</b>",
                ok: "{_ Delete _}",
                on_confirm: function() {
                    $(self).closest('li.menu-item').fadeOut(500, function() {
                        $(this).remove();
                        $sorter.trigger('sortupdate')
                    });
                },
                is_danger: true
            });

            cotonic.broker
                .call("bridge/origin/model/menu/get/is_menu_item_delete_safe/" ++ page_id)
                .then((m) => {
                    if (m.payload.result) {
                        $('#menu-delete-undo-text').hide();
                    } else {
                        $('#menu-delete-undo-text').show();
                    }
                });
		{% endif %}
	} else if (where == 'copy') {
		z_transport("mod_menu", "ubf", {
			cmd: "copy",
			id: $menu_item.children('div').data('page-id'),
            template: options.item_template || ""
		});
	} else {
		window.zMenuEditDone = function(v) {
            var m = {
                cmd: "menu-edit-done",
                payload: {
                    where: where,
                    object_id: v.object_id,
                    sorter: $sorter,
                    menuedit: $menuedit,
                    menu_item: $menu_item,
                    options: options
                }
            }
            menu_insert_queue.push(m);
            menu_dequeue();
		};
		z_event("admin-menu-select", {tab: "{{ connect_tab|default:"find" }}"});
	}
	e.preventDefault();
});

window.zMenuInsertAfter = function(after_id, html) {
	var $menu_item = $('#{{ menu_id }} div[data-page-id='+after_id+']').closest('li.menu-item');
	$html = $(html);
	$html.insertAfter($menu_item);
	$('#{{ in_sorter }}').trigger('sortupdate');
    cotonic.broker.publish("menu/insert", {
        menu_id: '{{ menu_id }}',
        id: $html.children('div').data('page-id')
    });
}

{% endjavascript %}
