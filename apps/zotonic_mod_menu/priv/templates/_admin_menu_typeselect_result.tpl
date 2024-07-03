{% for mid, _rank in result %}

<li class="header" id="{{ #menu.mid }}" data-nestedsortable-receive="{{ mid }}">
	{% include "_menu_edit_item.tpl" id=mid %}
</li>
{% draggable id=#menu.mid to_sorter=["menu-",id|to_binary] helper='clone' %}

{% empty %}

	<li class="suggestions-result"><a href="#">Nothing found.</a></li>

{% endfor %}
