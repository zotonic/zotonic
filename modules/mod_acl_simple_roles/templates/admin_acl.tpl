{% extends "admin_base.tpl" %}

{% block title %} Admin Access Control {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Access Control - Roles Overview</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text="make a new acl role" action={dialog_new_rsc cat="acl_role" nocatselect} %}
			</div>

			<hr />
			{% endif %}

			<p>Access control controls what an user is allowed to do and see.  The roles define different groups of rights.  Users can be made member of multiple roles.</p>
			
			<h3 class="above-list">ACL role overview</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-10">View all</span>
					<span class="zp-60">Rights</span>
					<span class="zp-10">Actions</span>
				</li>

			{% for title, id in m.search[{all_bytitle cat="acl_role"}] %}
				<li id="{{ #li.id }}">
					{% with m.rsc[id].acl as acl %}
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-20">{{ title|default:"&nbsp;" }}</span>
						<span class="zp-10">{{ acl.view_all|yesno:"view all,&mdash;" }}</span>
						<span class="zp-60">
							<strong>Categories</strong>:
							{% for cat in acl.categories %}
								{{ cat }}{% if not forloop.last %}, {% endif %}
							{% endfor %}<br/>
							<strong>Modules</strong>:
							{% for mod in acl.modules %}
								{{ mod }}{% if not forloop.last %}, {% endif %}
							{% endfor %}
						</span>
						<span class="zp-10">
							{% button disabled=p.is_protected text="delete" action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
					{% endwith %}
				</li>
			{% empty %}
				<li>
					No ACL roles found.
				</li>
			{% endfor %}
			</ul>

		</div>
	</div>
{% endwith %}
{% endblock %}
