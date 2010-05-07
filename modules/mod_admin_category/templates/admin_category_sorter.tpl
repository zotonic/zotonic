{% extends "admin_base.tpl" %}

{% block title %}Category Hierarchy{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Page Categories</h2>

			{% if editable %}
				{% button text="Make a new category" action={dialog_category_add on_success={reload}} %}
			{% endif %}
			
			<hr class="clear" />

			<p>Categories are used to categorize all pages. Every page belongs to exactly one category. The categories are defined in a hierarchy. Here you can change that hierarchy.</p>

		
			<div id="category-sorter" class="clear zp-67">
				{% include "_admin_category_sorter.tpl" %}
			</div>
			
			<div id="sidebar" class="zp-33">
				<h3 class="above-list">How does this work?</h3>
				<div class="item"><p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p></div>
			</div>
		</div>
	</div>
{% endwith %}
{% endblock %}