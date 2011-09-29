{% extends "admin_base.tpl" %}

{% block title %}{_ Category Hierarchy _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Page Categories _}</h2>

{#
			{% if editable %}
				{% button text="Make a new category" action={dialog_category_add on_success={reload}} %}
			{% endif %}
			
			<hr class="clear" />
#}
			<div id="category-sorter" class="clear zp-50">
				{% include "_admin_category_sorter.tpl" %}
			</div>
			
			<div id="sidebar" class="zp-50">
				<h3 class="above-list">{_ How does this work? _}</h3>
				<div class="item">
					<div class="zp-30">
						{% include "_menu_trash.tpl" %}

						<ul id="sortable-new" class="short-list ui-sortable" title="{_ Drag to add a new page _}">
							<li class="header" id="page-new-category">
								<div class="clearfix">
									<span class="grippy"><img src="/lib/images/grippy.png" alt="{_ Drag me _}" /></span>
									<span>{_ New category _}</span>
								</div>
							</li>
							{% draggable id="page-new-category" to_sorter="category" helper='clone' %}
						</ul>
					</div>

					<div class="zp-10">&nbsp;</div>
					
					<div class="zp-60">
						<p class="notification notice">{_ Categories are used to categorize all pages. Every page belongs to exactly one category.<br/>The categories are defined in a hierarchy. Here you can change that hierarchy. _}</p>
						<p>
							{_ Drag categories to the place where you want them in the hierarchy. _}
						</p>
						<p>
							{_ Drag categories to the trash to remove them. _} 
							{_ Drag the <strong>New category</strong> to add a new category. _}
						</p>
					</div>
					
					<div class="clearfix"></div>
				</div>
			</div>
		</div>
	</div>
{% endwith %}
{% endblock %}