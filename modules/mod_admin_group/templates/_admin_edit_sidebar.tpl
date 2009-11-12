{% if m.rsc[id].is_a.group %}

{% sortable id="sort-group_members" %}

<div class="item-wrapper" id="sort-group_members">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">Group members</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item clearfix admin-form">
		<div class="notification notice">
			List, add or remove group members. <a href="javascript:void(0)" class="do_dialog {title: 'Help about group members.', text: 'Group <strong>members</strong> can edit content owned by the group, they can also edit the group page.  <strong>Leaders</strong> can add or remove members.  <strong>Observers</strong> can see everything in a group but not change anything.', width: '450px'}">Need more help?</a>
		</div>
		
		{% if id|member:m.acl.member or id|member:m.acl.observer or m.acl.leader %}
			<p>You are: <strong>{% if id|member:m.acl.member %}member{% endif %} {% if id|member:m.acl.leader %}leader {% endif %} {% if id|member:m.acl.observer %}observer{% endif %}</strong></p>
		{% endif %}
		
		{# todo: add a 'save' action #}
		{% button text="View group members" action={redirect dispatch="admin_group_members" id=id} %}
	</div>
</div>

{% endif %}
