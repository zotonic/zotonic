{% with m.acl.is_admin as editable %}
		
	<div class="padding">
		<h3 class="above-list">Category overview</h3>
		<ul class="short-list categories">
			{% for id, depth, nbsp, name in m.category.all_flat_meta %}

				{% if editable %}
					{% droppable id=#before.id tag="b-"|append:id %}
					{% droppable id=#cat.id tag="t-"|append:id %}
					{% draggable id=#cat.id tag="t-"|append:id  clone %}
				{% endif %}

				<li id="{{ #before.id }}" class="line depth-{{ depth }}"></li>

				<li id="{{ #cat.id }}" class="depth-{{ depth }}">
					<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="cat-title">{{ m.rsc[id].title|default:name }}</span>
					</a>
				</li>
			{% endfor %}

			{% if editable %}
				{% droppable id=#last tag="end" %}
			{% endif %}

			<li id="{{ #last }}" class="line"></li>
		</ul>
	</div>
{% endwith %}
