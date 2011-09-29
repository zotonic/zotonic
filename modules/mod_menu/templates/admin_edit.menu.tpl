{% extends "admin_edit.tpl" %}

{% block admin_edit_form_pre %}
{# Edit the menu #}
<div class="zp-50">
	<div class="padding">
		<div id="menu-editor">
			{% include "_admin_menu_menu_view.tpl" id=id %}
		</div>
	</div>
</div>

<div class="item-wrapper search-nav-items zp-50">
	<h3 class="above-item">{_ Search for a page _}</h3>
	
	<div class="item">
		<div class="zp-20">
			{% include "_menu_trash.tpl" %}
			
			<ul id="sortable-new" class="short-list ui-sortable" title="{_ Drag to add a new page _}">
				<li class="header" id="page-new-text" data-nestedsortable-receive="new">
					<div class="clearfix">
						<span class="grippy"><img src="/lib/images/grippy.png" alt="{_ Drag me _}" /></span>
						<span>{_ New page _}</span>
					</div>
				</li>
				{% draggable id="page-new-text" to_sorter=["menu-",id|make_list] helper='clone' %}
			</ul>
		</div>
		
		<div class="zp-80">
			<p id="drag-explanation">
				{_ Type your search terms to find pages. Then drag them on to the panel on your left. _}
				{_ Drag items from the menu to the trash to remove them. _} 
				{_ Drag the <strong>New page</strong> to add a new page. _}
			</p>
		
			<div class="form-item autocomplete-wrapper clear">
				<input id="{{#input}}" class="autocompleter" type="text" value="" />
				<ul id="{{#suggestions}}" class="short-list ui-sortable"></ul>
			</div>
		
			{% wire id=#input
			type="keyup"
			action={typeselect target=#suggestions template="_admin_menu_typeselect_result.tpl" id=id}
			%}
		</div>
		
		<div class="clearfix">
		</div>
	</div>
</div>

{% endblock %}
