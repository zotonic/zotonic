{% extends "admin_base.tpl" %}

{% block title %}{_ Admin Access Control _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Access Control - Roles Overview _}</h2>

			{% if editable %}
			<div class="clearfix">
				{% button text=_"make a new acl role" action={dialog_new_rsc cat="acl_role" nocatselect} %}
			</div>

			<hr />
			{% endif %}

			<p>{_ Access control controls what an user is allowed to do and see.  The roles define different groups of rights.  Users can be made member of multiple roles. _}</p>
			
			<h3 class="above-list">{_ ACL role overview _}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">{_ Title _}</span>
					<span class="zp-10">{_ View all _}</span>
					<span class="zp-60">{_ Rights _}</span>
					<span class="zp-10">{_ Actions _}</span>
				</li>

			{% for title, id in m.search[{all_bytitle cat="acl_role"}] %}
				<li id="{{ #li.id }}">
					{% with m.rsc[id].acl as acl %}
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-20">{{ title|default:"&nbsp;" }}</span>
						<span class="zp-10">{{ acl.view_all|yesno:_"view all,&mdash;" }}</span>
						<span class="zp-60">
							<strong>{_ Categories _}</strong>:
							{% for cat in acl.categories %}
								{{ cat }}{% if not forloop.last %}, {% endif %}
							{% endfor %}<br/>
							<strong>{_ Modules _}</strong>:
							{% for mod in acl.modules %}
								{{ mod }}{% if not forloop.last %}, {% endif %}
							{% endfor %}
						</span>
						<span class="zp-10">
							{% button disabled=p.is_protected text=_"delete" action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}
							{% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
					{% endwith %}
				</li>
			{% empty %}
				<li>
					{_ No ACL roles found. _}
				</li>
			{% endfor %}
			</ul>

		</div>
	</div>
{% endwith %}
{% endblock %}
