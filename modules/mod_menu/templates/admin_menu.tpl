{% extends "admin_base.tpl" %}

{% block title %}Menu{% endblock %}

{% block content %}
<div id="content" class="zp-85">
	<div class="block clearfix">

		<h2>Zotonic Menu Editor</h2>

		<p>
			Here you can change the menu of your site.  Select pages on the right hand side and drag them to the menu on the left.
		</p>

		<div class="zp-33">
			<div class="padding">
			<div id="menu-editor">
				{% include "_admin_menu_menu_view.tpl" %}
			</div>
			</div>
		</div>
		
		<div class="item-wrapper search-nav-items zp-33">
			<div class="padding">
				<h3 class="above-item">Search for a page</h3>
				<div class="item">
					<p>Type your search terms to find pages. Then drag them on to the panel on your left.</p>

					<div class="form-item autocomplete-wrapper clear">
						<input id="{{#input}}" class="autocompleter" type="text" value="" />
						<ul id="{{#suggestions}}" class="short-list"></ul>
					</div>

					{% wire id=#input
						type="keyup" 
						action={typeselect target=#suggestions template="_admin_menu_typeselect_result.tpl"}
					%}
				</div>
			</div>
		</div>
		
		<div class="item-wrapper zp-33">
			<h3 class="above-item">How does this work?</h3>
			<div class="item">
				<p>
					Type the name of the page you want in your menu in the box left of this note. 
					When you get a result, you can drag that item onto the menu pane on the left side of the page.
				</p>
				<p>
					You can also move the menu items around to manage the way your navigations looks.
				</p>
				<p>
					A menu is at most two levels deep.
				</p>
			</div>
		</div>
	</div>
</div>
{% endblock %}
