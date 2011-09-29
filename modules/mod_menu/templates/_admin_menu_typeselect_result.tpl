{% for mid, _rank in result %}

<li class="header" id="{{ #m.mid }}" data-nestedsortable-receive="{{ mid }}">
	{% include "_menu_edit_item.tpl" id=mid %}
</li>
{% draggable id=#m.mid to_sorter=["menu-",id|make_list] helper='clone' %}

{#
	<li>
	    <div>
		{% with m.rsc[id] as r %}
			<a id="{{ #connect.id }}" href="#drag-item">{{ r.title }} (in <em>{{ m.rsc[r.category_id].title }})</em></a>
		{% endwith %}
		{% draggable id=#connect.id clone tag=["new", id] %}
	</li>
#}

{% empty %}

	<li class="suggestions-result"><a href="#">Nothing found.</a></li>

{% endfor %}
