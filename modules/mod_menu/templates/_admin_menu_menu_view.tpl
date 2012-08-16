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

{% include "_menu_edit_scripts.tpl" menu_id=#menu in_sorter="menu" %}
