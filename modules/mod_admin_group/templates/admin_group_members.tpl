{% extends "admin_base.tpl" %}

{% block title %} Admin Group Members {% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2><em>Members of the group</em> {{ m.rsc[id].title }}</h2>

			<div class="clearfix">
				{% button text="Add member" action={dialog_group_member_add id=id} %}
				{% button text="Edit group" action={redirect dispatch="admin_edit_rsc" id=id} %}
				{% button text="View all groups" action={redirect dispatch="admin_group"} %}
			</div>

		{% with m.acl.observer as observer %}
		{% with m.acl.leader as leader %}
		{% with m.acl.member as member %}

			<h3 class="above-list">Leaders</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-10">Category</span>
					<span class="zp-30">Actions</span>
				</li>
				{% for mid in m.group.leaders[id] %}
				<li id="{{ #li.mid }}">
					<a href="{% url admin_edit_rsc id=mid %}" class="clearfix">
						<span class="zp-20">{{ m.rsc[mid].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-10">{{ m.rsc[mid].category.name }}</span>
						<span class="zp-30">
							{% button text="remove membership" action={dialog_group_member_delete id=id member_id=mid on_success={slide_fade_out target=#li.mid}} %}
							{% button text="change membership" action={dialog_group_member_edit id=id member_id=mid on_success={reload}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=mid} %}
						</span>
					</a>
				</li>
				{% empty %}
				<li>
					No leaders.
				</li>
				{% endfor %}
			</ul>

			<h3 class="above-list">Observers</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-10">Category</span>
					<span class="zp-10">Actions</span>
				</li>
				{% for mid in m.group.observers[id] %}
				<li id="{{ #li.mid }}">
					<a href="{% url admin_edit_rsc id=mid %}" class="clearfix">
						<span class="zp-20">{{ m.rsc[mid].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-10">{{ m.rsc[mid].category.name }}</span>
						<span class="zp-30">
							{% button text="remove membership" action={dialog_group_member_delete id=id member_id=mid on_success={slide_fade_out target=#li.mid}} %}
							{% button text="change membership" action={dialog_group_member_edit id=id member_id=mid on_success={reload}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=mid} %}
						</span>
					</a>
				</li>
				{% empty %}
				<li>
					No observers.
				</li>
				{% endfor %}
			</ul>
			
			<h3 class="above-list">Members</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Title</span>
					<span class="zp-10">Category</span>
					<span class="zp-10">Actions</span>
				</li>
				{% for mid in m.group.members[id] %}
					<li id="{{ #li.mid }}">
					<a href="{% url admin_edit_rsc id=mid %}" class="clearfix">
						<span class="zp-20">{{ m.rsc[mid].title|striptags|default:"<em>untitled</em>" }}</span>
						<span class="zp-10">{{ m.rsc[mid].category.name }}</span>
						<span class="zp-30">
							{% button text="remove membership" action={dialog_group_member_delete id=id member_id=mid on_success={slide_fade_out target=#li.mid}} %}
							{% button text="change membership" action={dialog_group_member_edit id=id member_id=mid on_success={reload}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=mid} %}
						</span>
					</a>
					</li>
				{% empty %}
				<li>
					No members.
				</li>
				{% endfor %}
			</ul>
			
		{% endwith %}
		{% endwith %}
		{% endwith %}

		</div>
	</div>
{% endblock %}