<div class="widget">
	<h3 class="widget-header">{_ Current menu _}</h3>
	<div class="widget-content">
		<ul class="tree-list do_menuedit" id="menu-{{ id }}" data-menuedit="connectWith: '#trash'">
			{% for mid, path, action in id.menu|menu_flat %}
			{% with forloop.counter as c %}
			{% if mid %}
			<li id="{{ #menu.c }}-{{ mid }}">
				{% include "_menu_edit_item.tpl" id=mid %}
				{% if action == `down` %}
				<ul>
					{% else %}
				</li>
				{% endif %}
				{% else %}
			</ul></li>
			{% endif %}
			{% endwith %}
			{% endfor %}
		</ul>
	</div>
</div>
