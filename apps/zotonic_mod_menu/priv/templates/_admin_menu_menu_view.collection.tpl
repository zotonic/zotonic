{# Sorting edge trees has an issue with nested collections. #}
{#
<div class="widget" id="{{ #menu }}">
	<h3 class="widget-header">
		{{ id.title }}
	</h3>
	<div class="widget-content">

        <span class="btn-group pull-right">
            <a href="#" class="btn btn-default dropdown-toggle" data-toggle="dropdown">{_ Add _} <span class="caret"></span></a>
            <ul class="dropdown-menu">
                <!-- dropdown menu links -->
                <li><a href="#" data-where="top">&uarr; {_ Add top _}</a></li>
                <li><a href="#" data-where="bottom">&darr; {_ Add bottom _}</a></li>
            </ul>
        </span>

		<p>
			{_ Click on <strong>Add</strong> to add pages. _}
			<br/>{_ Drag items in the list up, down to structure the collection. _}
		</p>

		{% block menu_tree %}
		<ul class="tree-list do_menuedit" id="collection-{{ id }}" data-menuedit='{ "maxLevels": 1 }'>
			{% for mid in id.o.haspart %}
				{% include "_menu_edit_item.tpl" c=forloop.counter id=mid %}
			{% endfor %}
		</ul>
		{% endblock %}
	</div>
</div>
{% include "_menu_edit_scripts.tpl" menu_id=#menu in_sorter="collection-"++id %}
#}

<div id="{{ #menu }}">
	<h3>
		{{ id.title }}

		<a href="#edit_id={{ id }}" class="btn pull-right">
			{_ Edit _}
		</a>
	</h3>
	<p class="text-muted">{{ id.category_id.title }}</p>
</div>
