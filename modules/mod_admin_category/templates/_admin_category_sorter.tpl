{% with m.acl.is_admin as editable %}
		
	<div class="padding">
		<h3 class="above-list">Category overview</h3>
		<ul id="category" class="category-list categories {% if editable %}do_menuedit{% endif %}" data-menuedit="connectWith: '#trash'">
			{% for mid, path, action in m.category.menu|menu_flat %}
				{% with forloop.counter as c %}
				{% if mid %}
					<li class="header" id="{{ #cat.mid }}">
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
{% endwith %}

