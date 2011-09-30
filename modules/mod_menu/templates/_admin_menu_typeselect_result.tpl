{% for mid, _rank in result %}

<li class="header" id="{{ #m.mid }}" data-nestedsortable-receive="{{ mid }}">
	{% include "_menu_edit_item.tpl" id=mid %}
</li>
{% draggable id=#m.mid to_sorter=["menu-",id|make_list] helper='clone' %}

{% empty %}

	<li class="suggestions-result"><a href="#">Nothing found.</a></li>

{% endfor %}
