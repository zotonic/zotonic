{% extends "admin_base.tpl" %}

{% block title %} Admin Groups {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>Zotonic Groups</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text="Make a new group" action={dialog_group_new title=""} %}
			</div>
			
			<hr />
			{% endif %}

			<p>
				All pages belong to a group. Users that are member of a group can edit pages belonging to that group.
			</p>
			
			<div class="clearfix">
				<div class="zp-50">
					<p>
						A <strong>group leader</strong> can add or remove members from a group.
						<br/>A <strong>group member</strong> can edit or add pages belonging to a group.
						<br/>A <strong>group observer</strong> can see all pages in a group but not change them.
					</p>
				</div>
				<div class="zp-50">
					<p>
						An <strong>administrator</strong> can change all pages and administer the sytem
						<br/>A <strong>supervisor</strong> can see all pages of all groups but not change them.
						<br/>A <strong>community publisher</strong> can publish pages visible to all members.
						<br/>A <strong>public publisher</strong> can publish pages visible to the whole world.
					</p>
				</div>
			</div>
				
		{% with m.acl.observer as observer %}
		{% with m.acl.leader as leader %}
		{% with m.acl.member as member %}
			
			<h3 class="above-list">Groups overview</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-5">You are Member</span>
					<span class="zp-5">You are Leader</span>
					<span class="zp-5">You are Observer</span>
					<span class="zp-10">Administrator</span>
					<span class="zp-10">Supervisor</span>
					<span class="zp-10">Community Publisher</span>
					<span class="zp-10">Public Publisher</span>
					<span class="zp-10">Actions</span>
				</li>

			{% for title, id in m.search[{all_bytitle cat="group"}] %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-20">{{ title|default:"<em>untitled</em>" }}</span>

						{# Show if the current user is member/admin/supervisor of this group #}
						<span class="zp-5">
							{% if id|member:member or id|member:leader %}√{% else %}&middot;{% endif %}
						</span>
						<span class="zp-5">
							{% if id|member:leader %}√{% else %}&middot;{% endif %}
						</span>
						<span class="zp-5">
							{% if id|member:observer %}√{% else %}&middot;{% endif %}
						</span>

						{# Show what right group members get #}
						{% with m.group[id] as group %}
							<span class="zp-10">
								{% if group.is_admin %}Administrator{% else %}&middot;{% endif %}
							</span>
							<span class="zp-10">
								{% if group.is_supervisor %}Supervisor{% else %}&middot;{% endif %}
							</span>
							<span class="zp-10">
								{% if group.is_community_publisher %}Community{% else %}&middot;{% endif %}
							</span>
							<span class="zp-10">
								{% if group.is_public_publisher %}Public{% else %}&middot;{% endif %}
							</span>
						{% endwith %}

						<span class="zp-20">
							{% button text="members" action={redirect dispatch="admin_group_members" id=id} %}
							{% button text="delete" disabled=m.rsc[id].is_protected action={dialog_group_delete id=id on_success={slide_fade_out target=#li.id}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No groups found.
				</li>
			{% endfor %}
			</ul>
			
		{% endwith %}
		{% endwith %}
		{% endwith %}

		</div>
	</div>
{% endwith %}
{% endblock %}